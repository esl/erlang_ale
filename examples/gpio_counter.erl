%%%-------------------------------------------------------------------
%%% @author Torben Hoffmann <>
%%% @copyright (C) 2013, Torben Hoffmann
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2013 by Torben Hoffmann <>
%%%-------------------------------------------------------------------
-module(gpio_counter).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(UP_PIN, 22).
-define(DOWN_PIN, 17).
-define(ONES_PIN, 24).
-define(TWOS_PIN, 23).

-record(state,
        { count = 0 :: 0..3}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ok = gpio:pin_init_input(?UP_PIN),
    ok = gpio:pin_init_input(?DOWN_PIN),
    ok = gpio:pin_set_int(?UP_PIN, rising),
    ok = gpio:pin_set_int(?DOWN_PIN, rising),
    ok = gpio:pin_init_output(?ONES_PIN, false),
    ok = gpio:pin_init_output(?TWOS_PIN, false),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gpio_interrupt, ?UP_PIN, rising},
            #state{count=N}=State)  when N < 3 ->
    N1 = N + 1,
    set_counter_pins(N1),
    {noreply, State#state{count=N1}};
handle_info({gpio_interrupt, ?DOWN_PIN, rising},
            #state{count=N}=State) when N > 0 ->
    N1 = N - 1,
    set_counter_pins(N1),
    {noreply, State#state{count=N1}};
handle_info({gpio_interrupt, _Pin, _Condition},
            State) ->
    {noreply, State}.

set_counter_pins(N1) ->
    <<Twos:1, Ones:1>> = <<N1:2>>,
    gpio:pin_write(?ONES_PIN, Ones),
    gpio:pin_write(?TWOS_PIN, Twos).

        

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



