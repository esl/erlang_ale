%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module contains functions for handle Erlang/ALE Gpio, I2C and SPI modules 
%% through a handler what controls these modules. This control means if the started
%% module had been started, for example a Gpio has been configured as input,
%% the Erlang process what is assigned for handle this operation, will restart automatically
%% if that process crashes for some reason. 
%% @end


-module(ale_handler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%===================================================================
%% Includes
%%===================================================================
-include("ale_type_def.hrl").
-include("ale_common.hrl").

%%===================================================================
%% Defines
%%===================================================================
-define(SUPERVISOR, erlang_ale_sup).
-define(SERVER, ?MODULE).
-define(TIMEOUT_FOR_OPERATION, 10000).

-type child_id()	::	term(). %% Not a pid()
-type mfargs()		::	{M :: module(), F :: atom(), A :: [term()] | undefined}.
-type restart()		::	permanent | transient | temporary.
-type shutdown()	::	brutal_kill | timeout().
-type worker()		::	worker | supervisor.
-type modules()		::	[module()] | dynamic.
-type child_spec()	::	{Id :: child_id(),
						 StartFunc :: mfargs(),
						 Restart :: restart(),
						 Shutdown :: shutdown(),
						 Type :: worker(),
						 Modules :: modules()}.

%% ====================================================================
%% ALE Handler functions
%% ====================================================================
%% Gpio related functions
-export([
		 gpio_read/1,
		 gpio_write/2,
		 gpio_set_int/2, gpio_set_int/3,
		 gpio_release/1
		]).

%% I2C related functions
-export([
		 i2c_stop/2,
		 i2c_write/3,
		 i2c_read/3
		 ]).

%% SPI related functions
-export([
		 spi_stop/1,
		 spi_transfer/3
		]).

-export([get_child_spec/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).

%% ====================================================================
%% Genserver record 
%% ====================================================================
-record(state, {
			  	drvPid,		%% pid(), pid of the driver process
			  	initialMFA,	%% mfa()
				drvInitMFA,	%% mfa()
				gpioDirection	%% undefined | pin_direction()
				}).
-type state()	::	{state, pid(), mfa(), mfa()}.

%% ====================================================================
%% @doc
%% Read the logical value of a Gpio. The Gpio driver process will start automatically
%% if that is not started yet.
%% @end
-spec gpio_read(pin() | {sup, pin()}) -> pin_state() | {ok, pid()} | {'error', 'reading_from_output_pin'} | {error, term()}.
%% ====================================================================
gpio_read({sup, Gpio}) ->
 	%% Start ALE handler if not started yet.
 	start_link({?MODULE, gpio_read, [Gpio]}, {?DRV_GPIO_MODULE, ?START_FUNC_DRV_MODULE, [Gpio, ?PIN_DIRECTION_INPUT]});
gpio_read(Gpio) when is_integer(Gpio) ->
	%% Start supervisor if not started yet.
	Res = case erlang_ale_sup:start_link() of
		{ok, _SupPid} ->
			ChildId = {gpio, Gpio},
			
			%% Check that GPIO os not used for opposite direction, such as OUTPUT for write mode. Reject if so.
			case get_child_spec(ChildId) of
				{ok, {ChildId, AleHandlerPidT, _Worker, _Modules}} ->
					%% GPIO is already used. Compare the directios.
					case gen_server:call(AleHandlerPidT, {get_gpio_direction}, ?TIMEOUT_FOR_OPERATION) of
						{ok, ?PIN_DIRECTION_INPUT} ->
							%% Ok, GPIO is used for the same IO direction
							{ok, AleHandlerPidT};
						{ok, PinDirection} ->
							%% GPIO is used for different direction. Must reject the request.
							{error, {'gpio_is_aready_used_for_write', {pinDirection, PinDirection}}}
					end;
				_-> %% Start child process if not started yet.
					case supervisor:start_child(?SUPERVISOR,
													  {ChildId, 
													   {?MODULE, gpio_read, [{sup,Gpio}]},
													   transient, 10, worker, [?MODULE]}) of
						{ok, PidT} ->
							{ok, PidT};
						{error, {already_started, PidT}} ->
							{ok, PidT};
						ERT ->
							{error, ERT}
					end
			end;
		ERT ->
		  {error, ERT}
	end,
	
	case Res of
		{ok, AleHandlerPid} ->
			%% Call in to the ale handler gen_server for do the real task.
			case catch gen_server:call(AleHandlerPid, {gpio_read}, ?TIMEOUT_FOR_OPERATION) of
				{ok, PinState} ->
					PinState;
				ER->ER
			end;
		ER->ER
	end;
gpio_read(Gpio) ->
	{error, {invalid_gpio, Gpio}}.

%% ====================================================================
%% @doc
%% Write the logical value of a Gpio. The Gpio driver process will start automatically
%% if that is not started yet.
%% @end
-spec gpio_write(pin(), pin_state()) -> ok | {ok, pid()} | {'error', 'reading_from_output_pin'} | {error, term()}.
%% ====================================================================
gpio_write({sup, Gpio}, PinState) ->
 	%% Start ALE handler if not started yet.
 	start_link({?MODULE, gpio_write, [Gpio, PinState]}, {?DRV_GPIO_MODULE, ?START_FUNC_DRV_MODULE, [Gpio, ?PIN_DIRECTION_OUTPUT]});
gpio_write(Gpio, PinState) when is_integer(Gpio), ((PinState == ?PIN_STATE_HIGH) or (PinState == ?PIN_STATE_LOW)) ->
	%% Start supervisor if not started yet.
	Res = case erlang_ale_sup:start_link() of
		{ok, _SupPid} ->
			ChildId = {gpio, Gpio},
			
			%% Check that GPIO os not used for opposite direction, such as INPUT for read mode. Reject if so.
			case get_child_spec(ChildId) of
				{ok, {ChildId, AleHandlerPidT, _Worker, _Modules}} ->
					%% GPIO is already used. Compare the directios.
					case gen_server:call(AleHandlerPidT, {get_gpio_direction}, ?TIMEOUT_FOR_OPERATION) of
						{ok, ?PIN_DIRECTION_OUTPUT} ->
							%% Ok, GPIO is used for the same IO direction
							{ok, AleHandlerPidT};
						{ok, PinDirection} ->
							%% GPIO is used for different direction. Must reject the request.
							{error, {'gpio_is_aready_used_for_read', {pinDirection, PinDirection}}}
					end;
				_-> %% Start child process if not started yet.
					case supervisor:start_child(?SUPERVISOR,
													  {ChildId, 
													   {?MODULE, gpio_write, [{sup,Gpio}, PinState]},
													   transient, 10, worker, [?MODULE]}) of
						{ok, PidT} ->
							{ok, PidT};
						{error, {already_started, PidT}} ->
							{ok, PidT};
						ERT ->
							{error, ERT}
					end
			end;
		ERT ->
		  {error, ERT}
	end,
	
	case Res of
		{ok, AleHandlerPid} ->
			case catch gen_server:call(AleHandlerPid, {gpio_write, PinState}, ?TIMEOUT_FOR_OPERATION) of
				ok ->
					ok;
				ER->ER
			end;
		ER->ER
	end;
gpio_write(Gpio, PinState) ->
	{error, {invalid_gpio_or_pinstate, {Gpio, PinState}}}.

%% ====================================================================
%% @doc
%% Configure a Gpio for able to handle interrupts. The Gpio IO direction
%% will be configured automacically.
%% @end
-spec gpio_set_int(pin(), interrupt_condition()) ->	'ok' | {'error', term()}.
%% ====================================================================
gpio_set_int(Gpio, IntCondition) when is_integer(Gpio) ->
	gpio_set_int(Gpio, IntCondition, self()).

%% ====================================================================
%% @doc
%% Configure a Gpio for able to handle interrupts. The Gpio IO direction
%% will be configured automacically.
%% @end
-spec gpio_set_int(pin(), interrupt_condition(), pid() | atom()) ->	{ok, pid()} | 'ok' | {'error', term()}.
%% ====================================================================
gpio_set_int(Gpio, IntCondition, Destination) when is_integer(Gpio) ->
	%% Start supervisor if not started yet.
	Res = case erlang_ale_sup:start_link() of
		{ok, _SupPid} ->
			ChildId = {gpio, Gpio},
			
			%% Check that GPIO os not used for opposite direction, such as OUTPUT for write mode. Reject if so.
			case get_child_spec(ChildId) of
				{ok, {ChildId, AleHandlerPidT, _Worker, _Modules}} ->
					%% GPIO is already used. Compare the directios.
					case gen_server:call(AleHandlerPidT, {get_gpio_direction}, ?TIMEOUT_FOR_OPERATION) of
						{ok, ?PIN_DIRECTION_INPUT} ->
							%% Ok, GPIO is used for the same purpose
							{ok, AleHandlerPidT};
						{ok, PinDirection} ->
							%% GPIO is used for different direction. Must reject the request.
							{error, {'gpio_is_aready_used_for_write', {pinDirection, PinDirection}}}
					end;
				_->	%% Start child process if not started yet.
					case supervisor:start_child(?SUPERVISOR,
													  {ChildId, 
													   {?MODULE, gpio_set_int, [{sup,Gpio}, IntCondition, Destination]},
													   transient, 10, worker, [?MODULE]}) of
						{ok, PidT} ->
							{ok, PidT};
						{error, {already_started, PidT}} ->
							{ok, PidT};
						ERT ->
							{error, ERT}
					end
			end;
		ERT ->
		  {error, ERT}
	end,
	
	case Res of
		{ok, AleHandlerPid} ->
			case catch gen_server:call(AleHandlerPid, {gpio_set_int, Gpio, IntCondition, Destination}, ?TIMEOUT_FOR_OPERATION) of
				ok ->
					ok;
				ER->ER
			end;
		ER->ER
	end;
gpio_set_int({sup, Gpio}, IntCondition, Destination) ->
 	%% Start ALE handler if not started yet.
 	start_link({?MODULE, gpio_set_int,  [Gpio, IntCondition, Destination]}, {?DRV_GPIO_MODULE, ?START_FUNC_DRV_MODULE, [Gpio, ?PIN_DIRECTION_INPUT]});
gpio_set_int(Gpio, _IntCondition, _Destination) ->
	{error, {invalid_gpio, Gpio}}.

%% ====================================================================
%% @doc
%% Release Gpio. 
%% @end
-spec gpio_release(pin()) -> ok | {error, term()}.
%% ====================================================================
gpio_release(Gpio) when is_integer(Gpio) ->
	case get_child_spec({gpio, Gpio}) of
		{ok, {ChildId, AleHandlerPid, _Worker, _Modules}} ->
			case catch gen_server:call(AleHandlerPid, {gpio_release}, ?TIMEOUT_FOR_OPERATION) of
				ok ->
					%% Terminate and delete child process.
					case supervisor:terminate_child(?SUPERVISOR, ChildId) of
						ok ->
							supervisor:delete_child(?SUPERVISOR, ChildId);
						ER->ER
					end;
					
				ER->ER
			end;
		no_child_process ->
			ok
	end;
gpio_release(Gpio) ->
	{error, {invalid_gpio, Gpio}}.

%% ====================================================================
%% @doc
%% Stop I2C driver.
%% @end
-spec i2c_stop(devname(), addr()) -> ok | {error, term()}.
%% ====================================================================
i2c_stop(DeviceName, HWAddress) ->
	case get_child_spec({i2c, DeviceName, HWAddress}) of
		{ok, {ChildId, AleHandlerPid, _Worker, _Modules}} ->
			case catch gen_server:call(AleHandlerPid, {i2c_stop}, ?TIMEOUT_FOR_OPERATION) of
				ok ->
					%% Terminate and delete child process.
					case supervisor:terminate_child(?SUPERVISOR, ChildId) of
						ok ->
							supervisor:delete_child(?SUPERVISOR, ChildId);
						ER->ER
					end;
				ER->ER
			end;
		no_child_process ->
			ok
	end.

%% ====================================================================
%% @doc
%% Write by into I2C device.
%% @end
-spec i2c_write(devname() | {sup, devname()}, addr(), data()) -> ok | {ok, pid()} | {error, term()}.
%% ====================================================================
i2c_write({sup, DeviceName}, HWAddress, Data) ->
 	%% Start ALE handler if not started yet.
 	start_link({?MODULE, i2c_write,  [DeviceName, HWAddress, Data]}, {?DRV_I2C_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, HWAddress]});
i2c_write(DeviceName, HWAddress, Data) ->
	%% Start supervisor if not started yet.
	Res = case erlang_ale_sup:start_link() of
		{ok, _SupPid} ->
			
			%% Start child process if not started yet.
			ChildId = {i2c, DeviceName, HWAddress},
			case supervisor:start_child(?SUPERVISOR,
											  {ChildId, 
											   {?MODULE, i2c_write, [{sup,DeviceName}, HWAddress, Data]},
											   transient, 10, worker, [?MODULE]}) of
				{ok, PidT} ->
					{ok, PidT};
				{error, {already_started, PidT}} ->
					{ok, PidT};
				ERT ->
					{error, ERT}
			end;
		ERT ->
		  {error, ERT}
	end,
	
	case Res of
		{ok, AleHandlerPid} ->
			case catch gen_server:call(AleHandlerPid, {i2c_write, Data}, ?TIMEOUT_FOR_OPERATION) of
				ok ->
					ok;
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Read data from I2C device.
%% @end
-spec i2c_read(devname() | {sup, devname()}, addr(), len()) -> {ok, data()} | {ok, pid()} | {error, term()}.
%% ====================================================================
i2c_read({sup, DeviceName}, HWAddress, Len) ->
 	%% Start ALE handler if not started yet.
 	start_link({?MODULE, i2c_read,  [DeviceName, HWAddress, Len]}, {?DRV_I2C_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, HWAddress]});
i2c_read(DeviceName, HWAddress, Len) ->
	%% Start supervisor if not started yet.
	Res = case erlang_ale_sup:start_link() of
		{ok, _SupPid} ->
			
			%% Start child process if not started yet.
			ChildId = {i2c, DeviceName, HWAddress},
			case supervisor:start_child(?SUPERVISOR,
											  {ChildId, 
											   {?MODULE, i2c_read, [{sup, DeviceName}, HWAddress, Len]},
											   transient, 10, worker, [?MODULE]}) of
				{ok, PidT} ->
					{ok, PidT};
				{error, {already_started, PidT}} ->
					{ok, PidT};
				ERT ->
					{error, ERT}
			end;
		ERT ->
		  {error, ERT}
	end,
	
	case Res of
		{ok, AleHandlerPid} ->
			case catch gen_server:call(AleHandlerPid, {i2c_read, Len}, ?TIMEOUT_FOR_OPERATION) of
				{ok, Data} ->
					{ok, Data};
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Stop SPI driver.
%% @end
-spec spi_stop(devname()) -> ok | {error, term()}.
%% ====================================================================
spi_stop(DeviceName) ->
	case get_child_spec({spi, DeviceName}) of
		{ok, {ChildId, AleHandlerPid, _Worker, _Modules}} ->
			case catch gen_server:call(AleHandlerPid, {spi_stop}, ?TIMEOUT_FOR_OPERATION) of
				ok ->
					%% Terminate and delete child process.
					case supervisor:terminate_child(?SUPERVISOR, ChildId) of
						ok ->
							supervisor:delete_child(?SUPERVISOR, ChildId);
						ER->ER
					end;
				ER->ER
			end;
		no_child_process ->
			ok
	end.

%% ====================================================================
%% @doc
%% Transfer data into/from SPI device.
%% @end
-spec spi_transfer(devname() | {sup, devname()}, list(), data()) -> {ok, data()} | {ok, pid()} | {error, term()}.
%% ====================================================================
spi_transfer({sup, DeviceName}, SpiOptions, Data) ->
 	%% Start ALE handler if not started yet.
 	start_link({?MODULE, spi_transfer, [DeviceName, SpiOptions, Data]}, {?DRV_SPI_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, SpiOptions]});
spi_transfer(DeviceName, SpiOptions, Data) ->
	%% Start supervisor if not started yet.
	Res = case erlang_ale_sup:start_link() of
		{ok, _SupPid} ->
			
			%% Start child process if not started yet.
			ChildId = {spi, DeviceName},
			case supervisor:start_child(?SUPERVISOR,
											  {ChildId, 
											   {?MODULE, spi_transfer, [{sup, DeviceName}, SpiOptions, Data]},
											   transient, 10, worker, [?MODULE]}) of
				{ok, PidT} ->
					{ok, PidT};
				{error, {already_started, PidT}} ->
					{ok, PidT};
				ERT ->
					{error, ERT}
			end;
		ERT ->
		  {error, ERT}
	end,
	
	case Res of
		{ok, AleHandlerPid} ->
			case catch gen_server:call(AleHandlerPid, {spi_transfer, Data}, ?TIMEOUT_FOR_OPERATION) of
				{ok, Data} ->
					{ok, Data};
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Start ALE handler.
%% @end
-spec start_link(mfa(), mfa()) -> {ok, pid()} | {error, term()}.
%% ====================================================================
start_link(InitialMFA, DrvInitMFA) ->
	%% Start ALE handler process
	gen_server:start_link(?MODULE, [InitialMFA, DrvInitMFA], [{timeout, ?TIMEOUT_FOR_OPERATION}]).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([InitialMFA, DrvInitMFA]) ->
	case start_driver_process(InitialMFA, DrvInitMFA) of
		{ok, DrvPid} ->
			GpioDirection = case DrvInitMFA of
								{?DRV_GPIO_MODULE, ?START_FUNC_DRV_MODULE, [_Gpio, PIN_DIRECTION_INPUT]} ->
									PIN_DIRECTION_INPUT;
								_->	undefined
							end,
								
    		{ok, #state{drvPid = DrvPid,
						initialMFA = InitialMFA,
						drvInitMFA = DrvInitMFA,
						gpioDirection = GpioDirection}};
		ER->{stop, ER}
	end.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({get_gpio_direction}, _From, State) ->
	{reply, {ok, State#state.gpioDirection}, State};

handle_call({gpio_read}, _From, State) ->
	  %% Read Gpio pin's logical state.
	  Reply = case catch erlang:apply(?DRV_GPIO_MODULE, read, [State#state.drvPid]) of
		  {'EXIT',R} ->
			  {error, {'EXIT',R}};
		  ResT-> ResT
	  end,
	{reply, Reply, State};
	
handle_call({gpio_write, PinState}, _From, State) ->
	%% Write logical value of Gpio.
	Reply = case catch erlang:apply(?DRV_GPIO_MODULE, write, [State#state.drvPid, PinState]) of
				ok ->
					ok;
				{error,R} ->
					{error,R};
				{'EXIT',R} ->
					{error, {'EXIT',R}}
			end,
	{reply, Reply, State};

handle_call({gpio_set_int, _Gpio, IntCondition, _Destination}, _From, State) ->
	%% Read GPIO value before set interrupt condition. This is needed, otherwise an unexpected inerrupt occurred on the GPIO.
	%% The interrupt is not a real interrupt, but GPIO driver or kernel has generates this.
	%% See the related issue: https://github.com/esl/erlang_ale/issues/26
	
	Reply = case catch erlang:apply(?DRV_GPIO_MODULE, read, [State#state.drvPid]) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		_->	%% Configure interrupt condition.
			case catch erlang:apply(?DRV_GPIO_MODULE, set_int, [State#state.drvPid, IntCondition]) of
				ok ->
					ok;
				{error, R} ->
					{error, R};
				{'EXIT',R} ->
					{error, {'EXIT',R}}
			end
	end,
	{reply, Reply, State};

handle_call({gpio_release}, _From, State) ->
	Reply = stop_driver_process(State),
	{reply, Reply, State};
	
handle_call({i2c_stop}, _From, State) ->
	{reply, stop_driver_process(State), State};
	
handle_call({i2c_write, Data}, _From, State) ->
	%% Write data into the I2C device.
	Reply = case catch erlang:apply(?DRV_I2C_MODULE, write, [State#state.drvPid, Data]) of
				ok ->
					ok;
				{error,R} ->
					{error,R};
				{'EXIT',R} ->
					{error, {'EXIT',R}}
			end,
	{reply, Reply, State};

handle_call({i2c_read, Len}, _From, State) ->
	%% Read data from the I2C device.
	Reply = case catch erlang:apply(?DRV_I2C_MODULE, read, [State#state.drvPid, Len]) of
						Data when is_binary(Data) ->
							{ok, Data};
						{error,R} ->
							{error,R};
						{'EXIT',R} ->
							{error, {'EXIT',R}}
			end,
	{reply, Reply, State};

handle_call({spi_transfer, Data}, _From, State) ->
	%% Transfer data from/to the SPI device.
	Reply = case catch erlang:apply(?DRV_SPI_MODULE, transfer, [State#state.drvPid, Data]) of
				DataReceived when is_binary(DataReceived) ->
					{ok, DataReceived};
				{error,R} ->
					{error,R};
				{'EXIT',R} ->
					{error, {'EXIT',R}}
			end,
	{reply, Reply, State};

handle_call({spi_stop}, _From, State) ->
	{reply, stop_driver_process(State), State};
	
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
	?DO_INFO("Genserver terminate function is called", [{reason, Reason}, {state, State}]),
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% @doc
%% Start driver process if not started yet.
%% Input:
%%		InitialMFA			: tuple, the MFA of the required operation.
%%		DrvInitMFA			: tuple, the MFA for init driver process
%% Output:
%%		{ok,pid()} | {error, term()}
%% @end
-spec start_driver_process(mfa(), mfa()) -> {ok,pid()} | {error, term()}.
%% ====================================================================
start_driver_process(InitialMFA, {DrvModule, DrvStartFunction, Arg}) ->
	Res = case catch erlang:apply(DrvModule, DrvStartFunction, Arg) of
			  {'EXIT',R} ->
				  {error, {'EXIT',R}};
			  
			  {ok, DrvPid} when is_pid(DrvPid) ->
				  case DrvModule of
					  ?DRV_GPIO_MODULE ->
						  %% Handle special case for interrupt handling on Gpio.
						  SpecialCaseRes = case InitialMFA of
											   {_Module, gpio_set_int, [_Gpio, _IntCondition, Destination]} ->
												   %% Register interrupt process
												   case erlang:apply(?DRV_GPIO_MODULE, register_int, [DrvPid, Destination]) of
													   ok ->
														   ok;
													   {error, R} ->
														   {error, R}
												   end;
											   _-> ok
										   end,
						  case SpecialCaseRes of
							  ok ->
								  {ok, DrvPid};
							  ER->ER
						  end;
					  
					  ?DRV_I2C_MODULE ->
						  {ok, DrvPid};
					  
					  ?DRV_SPI_MODULE ->
						  {ok, DrvPid};
					  
					  _-> %% Unsupported scenario.
						  %% FIXME
						  {error, {unsupported_drv_module, DrvModule}}
				  end;
			  ER->ER
		  end,
	
	case Res of
		{ok, DrvPidT} ->
			?DO_INFO("ALE driver process has been started and registered successfully.",
					 [
					  {initialMFA, InitialMFA},
					  {drvModule, DrvModule},
					  {drvStartFunction, DrvStartFunction},
					  {drvStartArgs, Arg},
					  {drvPid, DrvPidT}]),
			
			{ok, DrvPidT};
		Error->
			?DO_ERR("Error occured when start ALE driver process.",
					[
					 {initialMFA, InitialMFA},
					 {drvModule, DrvModule},
					 {drvStartFunction, DrvStartFunction},
					 {drvStartArgs, Arg},
					 {reason, Error}]),
			Error
	end.

%% ====================================================================
%% @doc
%% Stop driver process.
%% Input:
%%		State	:	state()
%% Output:
%%		ok | {error, term()}
%% @end
-spec stop_driver_process(state()) -> ok | {error, term()}.
%% ====================================================================
stop_driver_process(State) ->
	case erlang:is_process_alive(State#state.drvPid) of
		true ->
			%% Process is alive.
			SpecialCaseRes = case erlang:element(1, State#state.drvInitMFA) of
								?DRV_GPIO_MODULE ->
									case State#state.initialMFA of
										{?MODULE, gpio_set_int, [_Gpio, _IntCondition, Destination]} ->
											%% Unregister interrupt process
											case catch erlang:apply(?DRV_GPIO_MODULE, unregister_int, [State#state.drvPid, Destination]) of
												ok ->
													ok;
												{error, R} ->
													{error, R}
											end;
										 _->%% Normal IO GPIO, so no interrupt has been configured.
											ok
									end;
								 ?DRV_I2C_MODULE ->
									 erlang:apply(?DRV_I2C_MODULE, ?STOP_FUNC_DRV_MODULE, [State#state.drvPid]),
									 ok;
								 ?DRV_SPI_MODULE ->
									 erlang:apply(?DRV_SPI_MODULE, ?STOP_FUNC_DRV_MODULE, [State#state.drvPid]),
									 ok;
								 DrvModuleT ->
									 {error, {unsupported_drv_module, DrvModuleT}}
							 end,
			case SpecialCaseRes of
				ok ->
					?DO_INFO("ALE driver has been released.",
							 [
							  {drvPid, State#state.drvPid},
							  {initialMFA, State#state.initialMFA},
							  {drvInitMFA, State#state.drvInitMFA}]),
					ok;
						
				ER->
					?DO_ERR("Failed to release ALE driver.",
							 [
							  {drvPid, State#state.drvPid},
							  {initialMFA, State#state.initialMFA},
							  {drvInitMFA, State#state.drvInitMFA},
							  {reason, ER}]),
					ER
			end;
		_->
			?DO_WAR("ALE driver does not started.",
					[{drvPid, State#state.drvPid},
					 {initialMFA, State#state.initialMFA},
					 {drvInitMFA, State#state.drvInitMFA}]),
			ok
	end.

%% ====================================================================
%% @doc
%% Find child in supervisor and resurns its child_spec().
%% @end
-spec get_child_spec({gpio, integer()} | {i2c, devname(), addr()} | {spi, devname(), addr()}) -> {ok, child_spec()} | no_child_process.
%% ====================================================================
get_child_spec(WhatToFind) ->
	%% Fing child process what belongs to GPIO.
	do_get_child_spec(WhatToFind, supervisor:which_children(?SUPERVISOR)).

do_get_child_spec(_, []) ->
	no_child_process;
do_get_child_spec({gpio, Gpio}, [{{gpio, Gpio}, AleHandlerPid, Worker, Modules} | _T])->
	%% Child has been found.
	{ok, {{gpio, Gpio}, AleHandlerPid, Worker, Modules}};
do_get_child_spec({i2c, DeviceName, HWAddress}, [{{i2c, DeviceName, HWAddress}, AleHandlerPid, Worker, Modules} | _T]) ->
	%% Child has been found.
	{ok, {{i2c, DeviceName, HWAddress}, AleHandlerPid, Worker, Modules}};
do_get_child_spec({spi, DeviceName}, [{{spi, DeviceName}, AleHandlerPid, Worker, Modules} | _T]) ->
	%% Child has been found.
	{ok, {{spi, DeviceName}, AleHandlerPid, Worker, Modules}};
do_get_child_spec(WhatToFind, [_|T]) ->
	do_get_child_spec(WhatToFind, T).
