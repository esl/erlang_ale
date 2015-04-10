%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module shows how to use MCP23S17 and MCP23017 IO
%% expander devices with Erlang/ALE.
%% @end


-module(ex_mcp23x17).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("mcp23x17.hrl").
-include("ale_common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(TIMEOUT_FOR_OPERATION, 10000).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 start_i2c_blinking_led/4, stop_i2c_blinking_led/0
		 ]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
				tref,
				pinstate
				}).

%% ====================================================================
%% @doc
%% Blinking a led connected to any Pin of MCP23017 (I2C) IO expander device.
%% Notes:
%%		- In this example i2c-1 device on Raspberry Pi has been used.
%% 
%% @end
-spec start_i2c_blinking_led(hw_addr(), mcp23x17_port(), mcp23x17_pin(), timer_in_msec()) -> ok | {error, term()}.
%% ====================================================================
start_i2c_blinking_led(HwAddr, Port, Pin, Timer) ->
	case gen_server:start({local, i2c_blinking_led}, ?MODULE, [{i2c_blinking_led, HwAddr, Port, Pin, Timer}], [{timeout, ?TIMEOUT_FOR_OPERATION}]) of
		{ok, _Pid} ->
			ok;
		{error,R} ->
			{error,R}
	end.

%% ====================================================================
%% @doc
%% Stop blinking Led on I2C device.
%% @end
-spec stop_i2c_blinking_led() -> ok.
%% ====================================================================
stop_i2c_blinking_led() ->
	case whereis(i2c_blinking_led) of
		P when is_pid(P) ->
			gen_server:call(i2c_blinking_led, {stop}, ?TIMEOUT_FOR_OPERATION);
		_->	ok
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
init([{i2c_blinking_led, HwAddr, Port, Pin, Timer}]) ->
	%% Set logical value 0 to Pin.
	mcp23x17:setup_io_logical_level(?MCP23X17_COMM_TYPE_I2C1, HwAddr, Port, Pin, ?MCP23X17_IO_LOGICAL_LOW),
	
	%% Start timerfor blinking the led conencted to the Pin.
	case timer:send_interval(Timer, self(), {do_blinking, i2c_blinking_led, ?MCP23X17_COMM_TYPE_I2C1, HwAddr, Port, Pin}) of
		{ok, TRef} ->
    		{ok, #state{tref = TRef, pinstate = ?MCP23X17_IO_LOGICAL_LOW}};
		ER->{stop, ER}
	end;
init([]) ->
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
handle_call({stop}, _From, State) ->
	timer:cancel(State#state.tref),
	{stop, normal, ok, State};

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
handle_info({do_blinking, i2c_blinking_led, CommType, HwAddr, Port, Pin}, State) ->
	NewPinState = case State#state.pinstate of
					  ?MCP23X17_IO_LOGICAL_LOW ->
						  %% Turn Led ON
						  ?MCP23X17_IO_LOGICAL_HIGH;
					  _-> ?MCP23X17_IO_LOGICAL_LOW
				end,
	mcp23x17:setup_io_logical_level(CommType,HwAddr,Port,Pin,NewPinState),
	{noreply, State#state{pinstate = NewPinState}};
	
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


