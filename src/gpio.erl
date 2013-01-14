-module(gpio).

%% @doc This module is an experiment on how to provide exclusive write
%% acces to a GPIO pin.
%% Only the process that has obtained exclusive write by initialising
%% the pin with this request can write to the pin. All others will be
%% told that they cannot write to it.
%% Advanced things: should the process go down the lock on the pin has
%% to be released.
%%
%% GPIO API:
%% init(Pin, Direction)
%% release(Pin)
%% write(Pin, Value)
%% read(Pin)
%% set_int(Pin, Condition) - Condition = raising | falling | both.


-behaviour(gen_server).


%% API to the GPIO pins
-export([pin_init_input/1,
         pin_init_output/2,
         pin_release/1,
         pin_write/2,
         pin_read/1,
         pin_set_int/2
        ]).

-export([start_link/0]).

         %% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-type pin_number()    :: pos_integer().
-type pin_exclusive() :: boolean().
-type pin_direction() :: 'input' | 'output'.

-record(pin_info,
        { number       :: pin_number(),
          direction    :: pin_direction(),
          owner = none :: 'none' | pid()
        }).

-record(state,
        { pins = [] :: [#pin_info{}]
        }).

%% API
pin_init_input(Pin) ->
    gen_server:call(?SERVER, {pin_initinput, Pin}).

pin_init_output(Pin, false) ->
    gen_server:call(?SERVER, {pin_init_output, Pin});
pin_init_output(Pin, true) ->
    Owner = self(),
    gen_server:call(?SERVER, {pin_init_output, Pin, Owner}).

pin_release(Pin) ->
    gen_server:call(?SERVER, {pin_release, Pin}).

pin_write(Pin, Value) ->
    Requester = self(),
    gen_server:call(?SERVER, {pin_write, Pin, Value, Requester}).

pin_read(Pin) ->
    gen_server:call(?SERVER, {pin_read, Pin}).

pin_set_int(Pin, Condition) ->
    Requester = self(),
    gen_server:call(?SERVER, {pin_set_int, Pin, Condition, Requester}).
    

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).



%% gen_server callbacks

init([]) ->
    {ok, #state{pins=[]}}.

handle_call({pin_init, Pin, Direction, Exclusive},
            _From,
            State) ->
    case orddict:is_key(Pin) of
        true ->
            Reply = {error, pin_already_initialised},
            {reply, Reply, State};
        false ->
            try sim_gpio:init(Pin, Direction) of
                {ok, _Pid} ->
                    Reply = ok,
                    NewPins = orddict:store(Pin, Exclusive, State#state.pins),
                    {reply, Reply, State#state{pins=NewPins}}
            catch
                _:_ ->
                    Reply = {error, pin_init_failed},
                    {reply, Reply, State}
            end
    end;
handle_call({pin_init_input, Pin}, _From, State) ->
    Direction = input,
    non_exclusive_pin_init(Pin, State, Direction);
handle_call({pin_init_output, Pin}, _From, State) ->
    Direction = output,
    non_exclusive_pin_init(Pin, State, Direction);
handle_call({pin_init_output, Pin, Owner}, _From, State) ->
    %% @todo should refactor this so non_exclusive_pin_init code is
    %% reused.
    Direction = output,
    case lists:keyfind(Pin, #pin_info.number, State#state.pins) of
        #pin_info{} ->
            Reply = {error, pin_already_initialised},
            {reply, Reply, State};
        false ->
            try simp_gpio:init(Pin, Direction) of
                {ok, _Pid} ->
                    PinInfo = #pin_info{number=Pin,
                                        direction=Direction,
                                        owner = Owner},
                    Reply = ok,
                    NewPins = [PinInfo | State#state.pins],
                    {reply, Reply, State#state{pins = NewPins}}
            catch
                _:_ ->
                    Reply = {error, pin_init_failed},
                    {reply, Reply, State}
            end
    end;
         
           
handle_call({pin_write, Pin, Value, Requester},
            _From,
            State) ->
    case lists:keyfind(Pin, #pin_info.number, State#state.pins) of
        #pin_info{direction=output,
                  owner=Owner} when Owner == none;
                                    Owner == Requester ->
            simp_gpio:write(Pin, Value)
    {reply, Reply, State}.

non_exclusive_pin_init(Pin, State, Direction) ->
    case lists:keyfind(Pin, #pin_info.number, State#state.pins) of
        #pin_info{} ->
            Reply = {error, pin_already_initialised},
            {reply, Reply, State};
        false ->
            try simp_gpio:init(Pin, Direction) of
                {ok, _Pid} ->
                    PinInfo = #pin_info{number=Pin,
                                        direction=Direction},
                    Reply = ok,
                    NewPins = [PinInfo | State#state.pins],
                    {reply, Reply, State#state{pins = NewPins}}
            catch
                _:_ ->
                    Reply = {error, pin_init_failed},
                    {reply, Reply, State}
            end
    end.

            
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

