%% @author ethrbh
%% @doc
%% This module contains an example to show how to use GPIO interrupts through
%% ale_handler.erl module.
%% 
%% @end


-module(ex_gpio_int).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(INT_PIN, 27).
-define(SW_BTN_PIN, 22).
%% ====================================================================
%% Includes
%% ====================================================================
-include("ale_type_def.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).
-export([set_int_condition/1, press_btn/0, release_btn/0]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ====================================================================
%% @doc
%% This function will set the interrupt condition.
%% @end
-spec set_int_condition(interrupt_condition()) -> ok | {error, term()}.
%% ====================================================================
set_int_condition(IntCondition)->
	case gen_server:call(?SERVER, {set_int_condition, IntCondition}, 5000) of
		ok ->
			ok;
		ER->{error, ER}
	end.

%% ====================================================================
%% @doc
%% Set ?SW_BTN_PIN to HIGH, thus simualte the press button action.
%% This should generate a rising edge interrupt if that is configured.
%% @end
-spec press_btn() -> ok | {error, term()}.
%% ====================================================================
press_btn()->
	case gen_server:call(?SERVER, {press_btn}, 5000) of
		ok ->
			ok;
		ER->{error, ER}
	end.
	

%% ====================================================================
%% @doc
%% Set ?SW_BTN_PIN to LOW, thus simualte the release button action.
%% This should generate a falling edge interrupt if that is configured.
%% @end
-spec release_btn() -> ok | {error, term()}.
%% ====================================================================
release_btn()->
	case gen_server:call(?SERVER, {release_btn}, 5000) of
		ok ->
			ok;
		ER->{error, ER}
	end.


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
				ignoreInterrupt	= true :: boolean()	%% There is a known issue an "extra" interrupt has been received
													%% immediately after interrupt has been configured.
													%% So keep this value to true, and change it to false when the interrupt has been received.
				}).

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
	%% Configure interrupt pin
	ale_handler:gpio_set_int(?INT_PIN, ?INT_COND_RISING, self()),
	
	%% Configure SW BTN pin. Set logical state to HIGH (1) will simulate
	%% the press button , set logal state to LOW (0) will similate the release button actions.
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
handle_call({set_int_condition, IntCondition}, _From, State) ->
	Reply = ale_handler:gpio_set_int(?INT_PIN, IntCondition, self()),
	{reply, Reply, State};

handle_call({press_btn}, _From, State)->
	case ale_handler:gpio_write(?SW_BTN_PIN, ?PIN_STATE_HIGH) of
		ok ->
			{reply, ok, State};
		ER->{reply, {error, ER}, State}
	end;

handle_call({release_btn}, _From, State)->
	case ale_handler:gpio_write(?SW_BTN_PIN, ?PIN_STATE_LOW) of
		ok ->
			{reply, ok, State};
		ER->{reply, {error, ER}, State}
	end;

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
handle_info({gpio_interrupt, Pin, Condition},State) ->
	case State#state.ignoreInterrupt of
		true ->
			%% Ignore the interrupt, because this is an interrupt after the interrupt driver has been configured.
			{noreply, State#state{ignoreInterrupt = false}};
		false ->
			error_logger:info_report(["Interrupt has been occurred.", {gpio, Pin}, {interrupt_condition, Condition}]),
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

