%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This file contains functions for handle Erlang/ALE Gpio, I2C and SPI modules 
%% through a handler what controls these modules. This control means if the started
%% module had been started, for example a Gpio has been configured as input,
%% the Erlang process what is assigned for handle this operation, will restart automatically
%% if that process crashes for some reason. 
%% @end


-module(ale_handler).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%===================================================================
%% Include ALE type definitions
%%===================================================================
-include("ale_type_def.hrl").

%%===================================================================
%% Defines
%%===================================================================
-define(SERVER, ?MODULE).
-define(TIMEOUT_FOR_OPERATION, 10000).

-define(DRV_GPIO_MODULE, gpio).
-define(DRV_I2C_MODULE, i2c).
-define(DRV_SPI_MODULE, spi).

-define(START_FUNC_DRV_MODULE, start).
-define(STOP_FUNC_DRV_MODULE, stop).

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
-export([start/0]).

%% Gpio related functions
-export([
		 gpio_read/1,
		 gpio_write/2,
		 gpio_set_int/2, gpio_set_int/3
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
start() ->
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
	start(),
	
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
	start(),
	
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
	start(),
	
	case catch gen_server:call(?SERVER, {gpio_set_int, Gpio, IntCondition, Destination}, ?TIMEOUT_FOR_OPERATION) of
		{'EXIT',R} ->
			{error, {'EXIT',R}};
		ok ->
			ok;
		ER->ER
	end;
gpio_set_int(Gpio, _IntCondition, _Destination) ->
	{error, {invalid_gpio, Gpio}}.


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
				{ok, Pid} ->
					%% Process is already registered for interrupt handling.

					%% Configure interrupt condition.
					case erlang:apply(?DRV_GPIO_MODULE, set_int, [Pid, IntCondition]) of
						ok ->
							ok;
						{error, R} ->
							{error, R}
					end;
				{error, R} ->
					{error, R}
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
	case get_driver_process(MonitorRef) of
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
%%		MFA | MonitorRef, where MFA is
%%									DrvModule			:	atom
%%									DrvStartFunction	:	atom
%%									Arg					:	list, [term()]
%%								MonitorRef is			:	reference(). See erlang:monitor/2.
%% Output:
%%		{ok,rALEHandler{}} | {error, term()}
%% ====================================================================
get_driver_process({DrvModule, _DrvStartFunction, Arg}) ->
	Key = case DrvModule of
			  ?DRV_GPIO_MODULE ->
				  [Gpio, PinDirection] = Arg,
				  {?DRV_GPIO_MODULE, Gpio, PinDirection};
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
	end;
get_driver_process(MonitorRef) ->
	%% Search for driver process by its MonitorRef.
	case ets:match_object(?ALE_HANDLER_TABLE, #rALEHandler{monitorRef = MonitorRef, _='_'}) of
		[R] when is_record(R, rALEHandler) ->
			{ok, R};
		Recs when is_list(Recs) ->
			{error, {driver_was_started_multiple_times, {monitorRef, {MonitorRef, Recs}}}};
		ER ->
			{error, ER}
	end.

%% ====================================================================
%% Start driver process if not started yet.
%% Input:
%%		InitialMFA			:	tuple, the MFA of initial setup of driver process.
%%		DrvModule			:	atom
%%		DrvStartFunction	:	atom
%%		Arg					:	list, [term()]
%% Output:
%%		{ok,pid()} | {ok,do_register_monitor,pid()} | {error, term()}
%% ====================================================================
start_driver_process(InitialMFA, DrvModule, DrvStartFunction, Arg) ->
	%% Start driver process if not started yet.
	case get_driver_process({DrvModule, DrvStartFunction, Arg}) of
		{ok,R} ->
			%% Driver process already started. No need to do anything.
			{ok, R#rALEHandler.drvPid};
		
		_->	%% Driver process does not started yet. Do it.
			Res = case catch erlang:apply(DrvModule, DrvStartFunction, Arg) of
				{'EXIT',R} ->
					{error, {'EXIT',R}};
				{ok, Pid} when is_pid(Pid) ->
					case DrvModule of
						?DRV_GPIO_MODULE ->
							[Gpio, PinDirection] = Arg,
							
							%% Handle special case for interrupt handling on Gpio.
							SpecialCaseRes = case InitialMFA of
												 {_Module, gpio_set_int, [_Gpio, _IntCondition, Destination]} ->
													 %% Register interrupt process
													 case erlang:apply(?DRV_GPIO_MODULE, register_int, [Pid, Destination]) of
														 ok ->
															 ok;
														 {error, R} ->
															 {error, R}
													 end;
												 _-> ok
											 end,
							case SpecialCaseRes of
								ok ->
									{ok, {?DRV_GPIO_MODULE, Gpio, PinDirection}, Pid};
								ER->ER
							end;
						_->	%% Unsupported scenario.
							%% FIXME
							{error, {unsupported_drv_module, DrvModule}}
					end;
				ER->ER
			end,
			
			case Res of
				{ok, Key, DrvPid} ->
					%% Start monitor to this process.
					MonitorRef = erlang:monitor(process, DrvPid),
					
					%% Save all in ETS.
					ets:insert(?ALE_HANDLER_TABLE, #rALEHandler{key = Key,
																initialMFA = InitialMFA,
																drvPid = DrvPid, 
																monitorRef = MonitorRef}),
					
					error_logger:info_report(["ALE driver process has been started and registered successfully.",
											  {drvModule, DrvModule},
											  {drvStartFunction, DrvStartFunction},
											  {drvStartArgs, Arg},
											  {drvPid, DrvPid},
											  {monitorRef, MonitorRef}]),
					
					{ok, DrvPid};
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
%% stop_driver_process(DrvPid) ->
%% 	case erlang:is_process_alive(DrvPid) of
%% 		true ->
%% 			%% Process is alive.
%% 			case ets:match_object(?ALE_HANDLER_TABLE, #rALEHandler{drvPid = DrvPid, _='_'}) of
%% 				[R] when is_record(R, rALEHandler) ->
%% 					{DrvModule,_,_} = R#rALEHandler.key,
%% 					case erlang:apply(DrvModule, ?STOP_FUNC_DRV_MODULE, [DrvPid]) of
%% 						ok ->
%% 							ok;
%% 						ER->{error, ER}
%% 					end;
%% 				Recs when is_list(Recs) ->
%% 					%% Upps, this is a major error, because driver was started multiple times with the same attributes,
%% 					%% what is not allowed.
%% 					{error, {driver_was_started_multiple_times, {DrvPid, Recs}}}
%% 			end;
%% 			
%% 		false ->
%% 			%% Process does not alive.
%% 			{error, {process_is_not_alive, DrvPid}}
%% 	end.
