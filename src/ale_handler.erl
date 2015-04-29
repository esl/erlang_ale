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
-define(SERVER, ?MODULE).
-define(TIMEOUT_FOR_OPERATION, 10000).

-record(rALEHandler, {
					  key,			%% tuple, {gpio, pin(), pin_direction()} | {i2c, devname(), addr()} | {spi, list()}
					  initialMFA,	%% tuple, MFA
					  drvPid,		%% pid(), pid of the driver process
                      monitorRef	%% pid(), the monitor process what monitors the driver process.
					  }).

-define(ALE_HANDLER_TABLE, ale_handler_table).
-define(ALE_HANDLER_TABLE_KEYPOS, 2).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

%% Gpio related functions
-export([
		 gpio_read/1,
		 gpio_write/2,
		 gpio_set_int/2, gpio_set_int/3,
		 gpio_release/1,
		 gpio_get_driver_process/1
 		 ]).

%% I2C related functions
-export([
		 %%i2c_init/2,
		 i2c_stop/2,
		 i2c_write/3,
		 i2c_read/3
		 ]).

%% SPI related functions
-export([
		 %%spi_init/2,
		 spi_stop/2,
		 spi_transfer/3
		]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% ====================================================================
%% @doc
%% Start ALE handler.
%% @end
%% ====================================================================
start_link() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% ALE handler server is alive. Do nothing here.
			{ok, Pid};
		_->	%% ALE handler does not alive, must start it first.
			gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, ?TIMEOUT_FOR_OPERATION}])
	end.

%% ====================================================================
%% @doc
%% Read the logical value of a Gpio. The Gpio driver process will start automatically
%% if that is not started yet.
%% @end
-spec gpio_read(pin()) -> pin_state() | {'error', 'reading_from_output_pin'} | {error, term()}.
%% ====================================================================
gpio_read(Gpio) when is_integer(Gpio) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {gpio_read, Gpio}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		{ok, PinState} ->
			PinState;
		ER->ER
	end;
gpio_read(Gpio) ->
	{error, {invalid_gpio, Gpio}}.

%% ====================================================================
%% @doc
%% Write the logical value of a Gpio. The Gpio driver process will start automatically
%% if that is not started yet.
%% @end
-spec gpio_write(pin(), pin_state()) -> ok | {'error', 'reading_from_output_pin'} | {error, term()}.
%% ====================================================================
gpio_write(Gpio, PinState) when is_integer(Gpio), ((PinState == ?PIN_STATE_HIGH) or (PinState == ?PIN_STATE_LOW)) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {gpio_write, Gpio, PinState}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		ok ->
			ok;
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
-spec gpio_set_int(pin(), interrupt_condition(), pid() | atom()) ->	'ok' | {'error', term()}.
%% ====================================================================
gpio_set_int(Gpio, IntCondition, Destination) when is_integer(Gpio) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {gpio_set_int, Gpio, IntCondition, Destination}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		ok ->
			ok;
		ER->ER
	end;
gpio_set_int(Gpio, _IntCondition, _Destination) ->
	{error, {invalid_gpio, Gpio}}.

%% ====================================================================
%% @doc
%% Release Gpio. 
%% @end
-spec gpio_release(pin()) -> ok | {error, term()}.
%% ====================================================================
gpio_release(Gpio) when is_integer(Gpio) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {gpio_release, Gpio}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		ok ->
			ok;
		ER->ER
	end;
gpio_release(Gpio) ->
	{error, {invalid_gpio, Gpio}}.

%% ====================================================================
%% @doc
%% Give the DriverPorcess Pid of GPIO if that is registered already. 
%% @end
-spec gpio_get_driver_process(pin()) -> {ok, pid()} | {error, term()}.
%% ====================================================================
gpio_get_driver_process(Gpio) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {gpio_get_driver_process, Gpio}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		{ok, DrvPid} ->
			{ok, DrvPid};
		ER->ER
	end.

%% %% ====================================================================
%% %% @doc
%% %% Initialize I2C driver.
%% %% @end
%% -spec i2c_init(devname(), addr()) -> ok | {error, term()}.
%% %% ====================================================================
%% i2c_init(DeviceName, HWAddress) ->
%% 	%% Start ALE handler if not started yet.
%% 	start_link(),
%% 	
%% 	case catch gen_server:call(?SERVER, {i2c_init, DeviceName, HWAddress}, ?TIMEOUT_FOR_OPERATION) of
%% 		{'EXIT',R} ->
%% 			{error, {'EXIT',R}};
%% 		ok ->
%% 			ok;
%% 		ER->ER
%% 	end.

%% ====================================================================
%% @doc
%% Stop I2C driver.
%% @end
-spec i2c_stop(devname(), addr()) -> ok | {error, term()}.
%% ====================================================================
i2c_stop(DeviceName, HWAddress) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {i2c_stop, DeviceName, HWAddress}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		ok ->
			ok;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Write by into I2C device.
%% @end
-spec i2c_write(devname(), addr(), data()) -> ok | {error, term()}.
%% ====================================================================
i2c_write(DeviceName, HWAddress, Data) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {i2c_write, DeviceName, HWAddress, Data}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		ok ->
			ok;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Read data from I2C device.
%% @end
-spec i2c_read(devname(), addr(), len()) -> {ok, data()} | {error, term()}.
%% ====================================================================
i2c_read(DeviceName, HWAddress, Len) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {i2c_read, DeviceName, HWAddress, Len}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		{ok, Data} ->
			{ok, Data};
		ER->ER
	end.

%% %% ====================================================================
%% %% @doc
%% %% Init SPI driver.
%% %% @end
%% -spec spi_init(devname(), list()) -> ok | {error, term()}.
%% %% ====================================================================
%% spi_init(DeviceName, SpiOptions) ->
%% 	%% Start ALE handler if not started yet.
%% 	start_link(),
%% 	
%% 	case catch gen_server:call(?SERVER, {spi_init, DeviceName, SpiOptions}, ?TIMEOUT_FOR_OPERATION) of
%% 		{'EXIT',R} ->
%% 			{error, {'EXIT',R}};
%% 		ok ->
%% 			ok;
%% 		ER->ER
%% 	end.

%% ====================================================================
%% @doc
%% Stop SPI driver.
%% @end
-spec spi_stop(devname(), list()) -> ok | {error, term()}.
%% ====================================================================
spi_stop(DeviceName, SpiOptions) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {spi_stop, DeviceName, SpiOptions}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		ok ->
			ok;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Transfer data into/from SPI device.
%% @end
-spec spi_transfer(devname(), list(), data()) -> {ok, data()} | {error, term()}.
%% ====================================================================
spi_transfer(DeviceName, SpiOptions, Data) ->
	%% Start ALE handler if not started yet.
	start_link(),
	
	case catch gen_server:call(?SERVER, {spi_transfer, DeviceName, SpiOptions, Data}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		{ok, Data} ->
			{ok, Data};
		ER->ER
	end.

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
init([]) ->
	%% Create ETS tables used by ALE handler.
	ets:new(?ALE_HANDLER_TABLE, [ordered_set,public,named_table,{keypos,?ALE_HANDLER_TABLE_KEYPOS}]),
	
    {ok, #state{}}.


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
handle_call({gpio_read, Gpio}, _From, State) ->
	%% Start driver process if not started yet.
	Reply = case start_driver_process({?MODULE, gpio_read, [Gpio]}, ?DRV_GPIO_MODULE, ?START_FUNC_DRV_MODULE, [Gpio, ?PIN_DIRECTION_INPUT]) of
				{ok, Pid} ->
					%% Read Gpio pin's logical state.
					erlang:apply(?DRV_GPIO_MODULE, read, [Pid]);
				
				{error,R} ->
					{error, R}
			end,
	{reply, Reply, State};
	
handle_call({gpio_write, Gpio, PinState}, _From, State) ->
	%% Start driver process if not started yet.
	Reply = case start_driver_process({?MODULE, gpio_write, [Gpio, PinState]}, ?DRV_GPIO_MODULE, ?START_FUNC_DRV_MODULE, [Gpio, ?PIN_DIRECTION_OUTPUT]) of
				{ok, Pid} ->
					%% Write logical value of Gpio.
					case erlang:apply(?DRV_GPIO_MODULE, write, [Pid, PinState]) of
						ok ->
							ok;
						{error,R} ->
							{error,R}
					end;
				
				{error,R} ->
					{error,R}
			end,
	{reply, Reply, State};

handle_call({gpio_set_int, Gpio, IntCondition, Destination}, _From, State) ->
	%% Start driver process if not started yet.
	Reply = case start_driver_process({?MODULE, gpio_set_int,  [Gpio, IntCondition, Destination]}, ?DRV_GPIO_MODULE, ?START_FUNC_DRV_MODULE, [Gpio, ?PIN_DIRECTION_INPUT]) of
				{ok, DrvPid} ->
					%% Process is already registered for interrupt handling.
					
					%% Read GPIO value before set interrupt condition. This is needed, otherwise an unexpected inerrupt occurred on the GPIO.
					%% The interrupt is not a real interrupt, but GPIO driver or kernel has generates this.
					%% See the related issue: https://github.com/esl/erlang_ale/issues/26
					
					erlang:apply(?DRV_GPIO_MODULE, read, [DrvPid]),
					
					%% Configure interrupt condition.
					case erlang:apply(?DRV_GPIO_MODULE, set_int, [DrvPid, IntCondition]) of
						ok ->
							ok;
						{error, R} ->
							{error, R}
					end;
				{error, R} ->
					{error, R}
			end,
	
	{reply, Reply, State};

handle_call({gpio_release, Gpio}, _From, State) ->
	case get_driver_process(gpio, Gpio) of
		{ok,R} ->
			Reply = stop_driver_process(R#rALEHandler.drvPid),
			{reply, Reply, State};
		
		ER->{reply, ER, State}
	end;

handle_call({gpio_get_driver_process, Gpio}, _From, State) ->
	case get_driver_process(gpio, Gpio) of
		{ok,R} ->
			{reply, {ok, R#rALEHandler.drvPid}, State};
		ER->{error,ER}
	end;
	
handle_call({i2c_init, DeviceName, HWAddress}, _From, State) ->
	Reply = case start_driver_process({?MODULE, i2c_init,  [DeviceName, HWAddress]}, ?DRV_I2C_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, HWAddress]) of
				{ok, _DrvPid} ->
					ok;
				{error, R} ->
					{error, R}
			end,
	{reply, Reply, State};

handle_call({i2c_stop, DeviceName, HWAddress}, _From, State) ->
	Reply = case get_driver_process(?DRV_I2C_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, HWAddress]) of
				{ok, R} ->
					stop_driver_process(R#rALEHandler.drvPid);
					
				ER->
					ER
			end,
	{reply, Reply, State};
	
handle_call({i2c_write, DeviceName, HWAddress, Data}, _From, State) ->
	Reply = case start_driver_process({?MODULE, i2c_write,  [DeviceName, HWAddress, Data]}, ?DRV_I2C_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, HWAddress]) of
				{ok, DrvPid} ->
					%% Write data into the I2C device.
					case erlang:apply(?DRV_I2C_MODULE, write, [DrvPid, Data]) of
						ok ->
							ok;
						{error,R} ->
							{error,R}
					end;
				{error, R} ->
					{error, R}
			end,
	{reply, Reply, State};

handle_call({i2c_read, DeviceName, HWAddress, Len}, _From, State) ->
	Reply = case start_driver_process({?MODULE, i2c_read,  [DeviceName, HWAddress, Len]}, ?DRV_I2C_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, HWAddress]) of
				{ok, DrvPid} ->
					%% Read data from the I2C device.
					case erlang:apply(?DRV_I2C_MODULE, read, [DrvPid, Len]) of
						Data when is_binary(Data) ->
							{ok, Data};
						{error,R} ->
							{error,R}
					end;
				{error, R} ->
					{error, R}
			end,
	{reply, Reply, State};
	
handle_call({spi_init, DeviceName, SpiOptions}, _From, State) ->
	Reply = case start_driver_process({?MODULE, spi_init,  [DeviceName, SpiOptions]}, ?DRV_SPI_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, SpiOptions]) of
				{ok, _DrvPid} ->
					ok;
				{error, R} ->
					{error, R}
			end,
	{reply, Reply, State};

handle_call({spi_transfer, DeviceName, SpiOptions, Data}, _From, State) ->
	Reply = case start_driver_process({?MODULE, spi_transfer,  [DeviceName, SpiOptions, Data]}, ?DRV_SPI_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, SpiOptions]) of
				{ok, DrvPid} ->
					%% Transfer data from/to the SPI device.
					case erlang:apply(?DRV_SPI_MODULE, transfer, [DrvPid, Data]) of
						DataReceived when is_binary(DataReceived) ->
							{ok, DataReceived};
						{error,R} ->
							{error,R}
					end;
				{error, R} ->
					{error, R}
			end,
	{reply, Reply, State};


handle_call({spi_stop, DeviceName, SpiOptions}, _From, State) ->
	Reply = case get_driver_process(?DRV_SPI_MODULE, ?START_FUNC_DRV_MODULE, [DeviceName, SpiOptions]) of
				{ok, R} ->
					stop_driver_process(R#rALEHandler.drvPid);
					
				ER->
					ER
			end,
	{reply, Reply, State};
	
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
handle_info({'DOWN', MonitorRef, Type, Object, Info}, State) ->
	%% One of the monitored process has been died. Must re-initiate that according to that initial setup.
	%% The MFA what must call for this can be found in the ?ALE_HANDLER_TABLE ETS table.
	
	error_logger:error_report(["An ALE driver process has been died.",
							   {monitorRef, MonitorRef},
							   {type, Type},
							   {object, Object},
							   {info, Info}]),
	
	%% Try find the MonitorRef belongs to which driver process, and what the codition of that.
	case get_driver_process(monitorRef, MonitorRef) of
		{ok, R} ->
			%% Get the MFA for initial setup. Execute that once the record has been deleted in ETS table.
			ets:delete(?ALE_HANDLER_TABLE, R#rALEHandler.key),
			
			error_logger:info_report(["Re-register ALE driver process.", 
									  {mfa, R#rALEHandler.initialMFA}]),
			
			{M,F,A} = R#rALEHandler.initialMFA,
			erlang:spawn(M, F, A),
			{noreply, State};
		{error,R} ->
			error_logger:error_report(["ALE driver process does not found in ETS, thus it is not possible restart that.",
									   {monitorRef, MonitorRef},
									   {reason, {error,R}}]),
			{noreply, State}
	end;
	
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
terminate(_Reason, _State) ->
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
%% Find driver process
%% Input:
%%		DrvModule			:	atom
%%		DrvStartFunction	:	atom
%%		Arg					:	list, [term()]
%% Output:
%%		{ok,rALEHandler{}} | {error, term()}
%% ====================================================================
get_driver_process(DrvModule, _DrvStartFunction, Arg) ->
	Key = case DrvModule of
			  ?DRV_GPIO_MODULE ->
				  [Gpio, PinDirection] = Arg,
				  {?DRV_GPIO_MODULE, Gpio, PinDirection};
			  
			  ?DRV_I2C_MODULE ->
				  [DeviceName, HWAddress] = Arg,
				  {?DRV_I2C_MODULE, DeviceName, HWAddress};
			  
			  ?DRV_SPI_MODULE ->
				  [DeviceName, SpiOptions] = Arg,
				  {?DRV_SPI_MODULE, DeviceName, SpiOptions};
			  
			  _-> %% Unsupported scenario.
				  %% FIXME
				  {error, {unsupported_drv_module, DrvModule}}
		  end,
	case Key of
		{error, R} ->
			{error, R};
		_->	case ets:lookup(?ALE_HANDLER_TABLE, Key) of
				[R] when is_record(R, rALEHandler) ->
					{ok, R};
				ER->{error, ER}
			end
	end.

get_driver_process(monitorRef, MonitorRef) ->
	%% Search for driver process by its MonitorRef.
	case ets:match_object(?ALE_HANDLER_TABLE, #rALEHandler{monitorRef = MonitorRef, _='_'}) of
		[R] when is_record(R, rALEHandler) ->
			{ok, R};
		Recs when is_list(Recs) ->
			{error, {driver_was_started_multiple_times, {monitorRef, {MonitorRef, Recs}}}};
		ER ->
			{error, ER}
	end;
get_driver_process(gpio, Gpio) ->
	%% Search for driver process by its gpio.
	case ets:match_object(?ALE_HANDLER_TABLE, #rALEHandler{key = {'_', Gpio, '_'}, _='_'}) of
		[R] when is_record(R, rALEHandler) ->
			{ok, R};
		Recs when is_list(Recs) ->
			{error, {driver_was_started_multiple_times, {gpio, {Gpio, Recs}}}};
		ER ->
			{error, ER}
	end.

%% ====================================================================
%% Start driver process if not started yet.
%% Input:
%%		InitialMFA			: tuple, the MFA of initial setup of driver process.
%%		DrvModule			: atom
%%		DrvStartFunction	: atom
%%		Arg					: list, [term()]
%% Output:
%%		{ok,pid()} | {ok,do_register_monitor,pid()} | {error, term()}
%% ====================================================================
start_driver_process(InitialMFA, DrvModule, DrvStartFunction, Arg) ->
	%% Start driver process if not started yet.
	case get_driver_process(DrvModule, DrvStartFunction, Arg) of
		{ok,R} ->
			%% Driver process already started. No need to do anything.
			{ok, R#rALEHandler.drvPid};
		
		_->	%% Driver process does not started yet. Do it.
			Res = case catch erlang:apply(DrvModule, DrvStartFunction, Arg) of
				{'EXIT',R} ->
					{error, {'EXIT',R}};
				{ok, DrvPid} when is_pid(DrvPid) ->
					case DrvModule of
						?DRV_GPIO_MODULE ->
							[Gpio, PinDirection] = Arg,
							
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
									{ok, {?DRV_GPIO_MODULE, Gpio, PinDirection}, DrvPid};
								ER->ER
							end;
						
						?DRV_I2C_MODULE ->
							[DeviceName, HWAddress] = Arg,
							{ok, {?DRV_I2C_MODULE, DeviceName, HWAddress}, DrvPid};
						
						?DRV_SPI_MODULE ->
							[DeviceName, SpiOptions] = Arg,
							{ok, {?DRV_SPI_MODULE, DeviceName, SpiOptions}, DrvPid};
						
						_->	%% Unsupported scenario.
							%% FIXME
							{error, {unsupported_drv_module, DrvModule}}
					end;
				ER->ER
			end,
			
			case Res of
				{ok, Key, DrvPidT} ->
					%% Start monitor to this process.
					MonitorRef = erlang:monitor(process, DrvPidT),
					
					%% Save all in ETS.
					ets:insert(?ALE_HANDLER_TABLE, #rALEHandler{key = Key,
																initialMFA = InitialMFA,
																drvPid = DrvPidT, 
																monitorRef = MonitorRef}),
					
					error_logger:info_report(["ALE driver process has been started and registered successfully.",
											  {drvModule, DrvModule},
											  {drvStartFunction, DrvStartFunction},
											  {drvStartArgs, Arg},
											  {drvPid, DrvPidT},
											  {monitorRef, MonitorRef}]),
					
					{ok, DrvPidT};
				Error->
					error_logger:error_report(["Error occured when start ALE driver process.",
											  {drvModule, DrvModule},
											  {drvStartFunction, DrvStartFunction},
											  {drvStartArgs, Arg},
											  {reason, Error}
											  ]),
					Error
			end
	end.

%% ====================================================================
%% Stop driver process.
%% Input:
%%		DrvPid	:	pid()
%% Output:
%%		ok | {error, term()}
%% ====================================================================
stop_driver_process(DrvPid) ->
	case erlang:is_process_alive(DrvPid) of
		true ->
			%% Process is alive.
			case ets:match_object(?ALE_HANDLER_TABLE, #rALEHandler{drvPid = DrvPid, _='_'}) of
				[R] when is_record(R, rALEHandler) ->
					SpecialCaseRes = case erlang:element(1, R#rALEHandler.key) of
										 ?DRV_GPIO_MODULE ->
											 case R#rALEHandler.initialMFA of
												 {?MODULE, gpio_set_int, [_Gpio, _IntCondition, Destination]} ->
													 %% Unregister interrupt process
													 case erlang:apply(?DRV_GPIO_MODULE, unregister_int, [R#rALEHandler.drvPid, Destination]) of
														 ok ->
															 {ok, ?DRV_GPIO_MODULE};
														 {error, R} ->
															 {error, R}
													 end;
												 _-> %% Normal IO GPIO, so no interrupt has been configured.
													 {ok, ?DRV_GPIO_MODULE}
											 end;
										 
										 ?DRV_I2C_MODULE ->
											 {ok, ?DRV_I2C_MODULE};
										 
										 ?DRV_SPI_MODULE ->
											 {ok, ?DRV_SPI_MODULE};
										 
										 DrvModuleT ->
											 {error, {unsupported_drv_module, DrvModuleT}}
									 end,
					case SpecialCaseRes of
						{ok, DrvModule} ->
							%% Stop monitor the driver process
							erlang:demonitor(R#rALEHandler.monitorRef, [flush, info]),
							
							%% Stop driver process
							erlang:apply(DrvModule, ?STOP_FUNC_DRV_MODULE, [R#rALEHandler.drvPid]),
							
							%% Delete record in ETS
							ets:delete(?ALE_HANDLER_TABLE, R#rALEHandler.key),
							
							error_logger:info_report(["ALE driver has been released.",
													  {drvPid, DrvPid},
													  {record_in_ets, R}]),
							ok;
								
						ER->
							error_logger:error_report(["Failed to release ALE driver.",
													   {drvPid, DrvPid},
													   {record_in_ets, R},
													   {reason, ER}]),
							ER
					end;
				
				[] ->
					error_logger:warning_report(["ALE driver does not started.",
											  {drvPid, DrvPid}]),
					ok;
				
				Recs when is_list(Recs) ->
					%% Upps, this is a major error, because driver was started multiple times with the same attributes,
					%% what is not allowed.
					{error, {driver_was_started_multiple_times, {DrvPid, Recs}}}
			end;
			
		false ->
			%% Process does not alive.
			{error, {process_is_not_alive, DrvPid}}
	end.
