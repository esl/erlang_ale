%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module shows how to use MCP7940n RTC devices with Erlang/ALE.
%% @end

-module(ex_mcp7940n).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
		 stop/0, 
		 pwr_status_change_subscribe/0, 
		 pwr_status_change_unsubscribe/0,
		 osc_status_change_subscribe/0,
		 osc_status_change_unsubscribe/0,
		 alarm_configure/0]).

%% ====================================================================
%% Defines
%% ====================================================================
-define(SERVER, ?MODULE).
-define(INT_PIN, 17).			%% This GPIO will recevies the interrupt from the RTC.
-define(INT_CONDITION, rising).
-define(ALARM_MODULE, ?RTC_ALARM_0_ID).
-define(GENERAL_GENSERVER_TIMEOUT, 10000).

%% ====================================================================
%% Includes
%% ====================================================================
-include("mcp7940n.hrl").

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% ====================================================================
%% @doc
%% Start test application. This will start RTC driver and will configure
%% Date and Time to the localtime in 24H time format.
%% @end
-spec start_link() ->  {ok, pid()}  | {error, term()}.
%% ====================================================================
start_link() ->
	case whereis(?SERVER) of
		P when is_pid(P) ->
			{ok, P};
		_->
			gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, ?GENERAL_GENSERVER_TIMEOUT}])
	end.

%% ====================================================================
%% @doc
%% Stop RTC test application.
%% @end
-spec stop() -> ok | {error, term()}.
%% ====================================================================
stop() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Process is already exists. Must stop it.
			gen_server:call(Pid, {stop}, infinity);
		
		_->	%% Process is not running.
			ok
	end. 

%% ====================================================================
%% @doc
%% Subscribe to PWR DOWN/UP events.
%% @end
-spec pwr_status_change_subscribe() -> ok | {error, term()}.
%% ====================================================================
pwr_status_change_subscribe() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {pwr_status_change_subscribe}, infinity);
		
		ER->%% Process is not running.
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Unsubscribe to PWR DOWN/UP events.
%% @end
-spec pwr_status_change_unsubscribe() -> ok | {error, term()}.
%% ====================================================================
pwr_status_change_unsubscribe() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {pwr_status_change_unsubscribe}, infinity);
		
		ER->%% Process is not running.
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Subscribe to OSCILLATOR change events.
%% @end
-spec osc_status_change_subscribe() -> ok | {error, term()}.
%% ====================================================================
osc_status_change_subscribe() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {osc_status_change_subscribe}, infinity);
		
		ER->%% Process is not running.
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Unsubscribe to OSCILLATOR change events.
%% @end
-spec osc_status_change_unsubscribe() -> ok | {error, term()}.
%% ====================================================================
osc_status_change_unsubscribe() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {osc_status_change_unsubscribe}, infinity);
		
		ER->%% Process is not running.
			{error, ER}
	end.

%% ====================================================================
%% @doc
%% Configure Alarm-0 module with the following conditions:
%%		alarm time			: localtime+1 min
%%		alarm mask			: full match (Sec,Min,Hour,Day of week, Date and Month)
%%		alarm_int_polarity	: HIGH
%% @end
%% ====================================================================
alarm_configure() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			gen_server:call(Pid, {alarm_configure}, infinity);
		
		ER->%% Process is not running.
			{error, ER}
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
	case mcp7940n:start(?RTC_HOUR_BIT_TIME_FORMAT_24H, ?RTC_WKDAY_BIT_VBATEN_EN) of
		{ok,_Pid} ->
 			%% Configure INT_PIN for input to able to receive interrupts from RTC.
 			Result = ale_handler:gpio_set_int(?INT_PIN, ?INT_CONDITION, self()),
			
			error_logger:info_report([string:concat(string:concat("INT_PIN (GPIO-", erlang:integer_to_list(?INT_PIN)), ") and interrupt conditions have been configured"), 
									  {result, Result}]),
			
    		{ok, #state{}};
		{error, ER} ->
			{stop, {error, ER}}
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
handle_call({stop}, _From, State) ->
	{stop, normal, ok, State};

handle_call({pwr_status_change_subscribe}, _From, State) ->
	Reply = mcp7940n:pwr_status_change_subscribe(),
	{reply, Reply, State};

handle_call({pwr_status_change_unsubscribe}, _From, State) ->
	Reply = mcp7940n:pwr_status_change_unsubscribe(),
	{reply, Reply, State};

handle_call({osc_status_change_subscribe}, _From, State) ->
	Reply = mcp7940n:oscillator_status_change_subscribe(),
	{reply, Reply, State};

handle_call({osc_status_change_unsubscribe}, _From, State) ->
	Reply = mcp7940n:oscillator_status_change_unsubscribe(),
	{reply, Reply, State};

handle_call({alarm_configure}, _From, State) ->
	{ok, {{Y,M,D},{H,Min,S}}} = mcp7940n:date_and_time_get(),
	Reply = mcp7940n:alarm_configure(?RTC_ALARM_0_ID, {{Y,M,D},{H,Min+1,S}}, ?RTC_ALMxWKDAY_BIT_ALMxMASK_ALL_MATCH, ?RTC_CTRL_BIT_ALM_Ax_EN, ?RTC_ALMxWKDAY_BIT_ALMPOL_HIGH),
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
handle_info({?NOTIFICATION_PWR_IS_BACK, PwrDownTS, PwrUpTS}, State) ->
	error_logger:info_report(["Main power of RTC is back", {powerDownTime, PwrDownTS}, {powerUpTime, PwrUpTS}]),
	{noreply, State};

handle_info({?NOTIFICATION_PWR_IS_LOST}, State) ->
	error_logger:info_report("Main power of RTC is lost"),
	{noreply, State};

handle_info({gpio_interrupt, Pin, Condition},State) ->
	%% Check which Alarm modue raised the interrupt
	case mcp7940n:alarm_interrupt_flag_check(?ALARM_MODULE) of
		{ok, ?RTC_ALMxWKDAY_BIT_ALMxIF_SET} ->
			%% Clear alarm flag in RTC module
			mcp7940n:alarm_interrupt_flag_clear(?ALARM_MODULE),
			
			error_logger:info_report(["RTC interrupt has been occurred.", {gpio, Pin}, {interrupt_condition, Condition}]);
		{ok, AlarmFlag} ->
			error_logger:error_report(["Unexpected interrupt has been occurred - alarm flag does not matches.", 
									   {gpio, Pin}, 
									   {interrupt_condition, Condition},
									   {expectedAlarmModule, ?ALARM_MODULE},
									   {expectedAlarmFlag,?RTC_ALMxWKDAY_BIT_ALMxIF_SET},
									   {readAlarmFlag, AlarmFlag},
									   {intGpioLogicalStatus, ale_handler:gpio_read(?INT_PIN)},
									   {'ALM0-IF', mcp7940n:alarm_interrupt_flag_check(?RTC_ALARM_0_ID)},
									   {'ALM1-IF', mcp7940n:alarm_interrupt_flag_check(?RTC_ALARM_1_ID)}]);
		ER->
			error_logger:error_report(["Unexpected interrupt has been occured on a NOT used Alarm module.", 
									   {gpio, Pin}, 
									   {interrupt_condition, Condition},
									   {expectedAlarmModule, ?ALARM_MODULE},
									   {reason,ER}])
	end,
	{noreply, State};

handle_info({oscillator_is_running}, State) ->
	error_logger:info_report("RTC oscillator is running"),
	{noreply, State};

handle_info({oscillator_is_not_running, Info}, State) ->
	error_logger:error_report(["RTC oscillator is not running", {info, Info}]),
	{noreply, State};

handle_info(Info, State) ->
	error_logger:info_report(["Unexpected message has been received", {msg, Info}, {state, State}]),
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


