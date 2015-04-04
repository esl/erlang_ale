%%%-------------------------------------------------------------------
%%% @author Torben Hoffmann <torben@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Limited
%%% @doc Simple example that shows how to program GPIO pins in
%%% Erlang/ALE.
%%% The idea is to use two LEDs to represent 2's and 1's and just
%%% count up and down between 0 and 3 by pressing buttens for +1 and
%%% -1.
%%% @end
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
        { count = 0 :: 0..3,
          up_pin,
          down_pin,
          ones_pin,
          twos_pin }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, UpPin} = gpio:start_link(?UP_PIN, input),
    {ok, DownPin} = gpio:start_link(?DOWN_PIN, input),
    ok = gpio:set_int(UpPin, rising),
    ok = gpio:set_int(DownPin, rising),
    ok = gpio:register_int(UpPin),
    ok = gpio:register_int(DownPin),
    {ok, OnesPin} = gpio:start_link(?ONES_PIN, output),
    {ok, TwosPin} = gpio:start_link(?TWOS_PIN, output),
    io:format("GPIO ~p counts up, and GPIO ~p counts down~n", [?UP_PIN, ?DOWN_PIN]),
    {ok, #state{up_pin=UpPin, down_pin=DownPin, ones_pin=OnesPin, twos_pin=TwosPin}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gpio_interrupt, ?UP_PIN, rising},
            #state{count=N}=State)  when N < 3 ->
    N1 = N + 1,
    set_counter_pins(State, N1),
    {noreply, State#state{count=N1}};
handle_info({gpio_interrupt, ?DOWN_PIN, rising},
            #state{count=N}=State) when N > 0 ->
    N1 = N - 1,
    set_counter_pins(State, N1),
    {noreply, State#state{count=N1}};
handle_info({gpio_interrupt, _Pin, _Condition},
            State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
set_counter_pins(#state{ones_pin=OnesPin, twos_pin=TwosPin}, N1) ->
    <<Twos:1, Ones:1>> = <<N1:2>>,
    gpio:write(OnesPin, Ones),
    gpio:write(TwosPin, Twos).



