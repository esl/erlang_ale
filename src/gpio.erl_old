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
%%
%% NB: when testing remember to start the gproc application first!

-ifdef(simulation_mode).
-define(GPIO_MODULE, sim_gpio).
-else.
-define(GPIO_MODULE, gpio_if).
-endif.


-behaviour(gen_server).


%% API to the GPIO pins
%% @todo add way to clear interrupt
-export([pin_init_input/1,
         pin_init_output/2,
         pin_release/1,
         pin_write/2,
         pin_read/1,
         pin_set_int/2
        ]).

-export([start_link/0,
         stop/0]).

         %% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-type pin_number()    :: pos_integer().
-type pin_exclusive() :: boolean().
-type pin_direction() :: 'input' | 'output'.


%% @todo consider if it would be better to split the interrupts field
%% into two: interrupt_state and listeners.
-record(pin_info,
        { number       :: pin_number(),
          direction    :: pin_direction(),
          owner = none :: 'none' | pid(),
          interrupts = none :: 'none'
                             | {sim_gpio:interrupt_condition(), [pid()]}
        }).

-record(state,
        { pins = [] :: [#pin_info{}]
        }).

%% API
pin_init_input(Pin) ->
    gen_server:call(?SERVER, {pin_init_input, Pin}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).
    

%% gen_server callbacks

init([]) ->
    {ok, #state{pins=[]}}.

handle_call({pin_init_input, Pin}, _From, State) ->
    Direction = input,
    pin_init(Pin, Direction, none, State);
handle_call({pin_init_output, Pin}, _From, State) ->
    Direction = output,
    pin_init(Pin, Direction, none, State);
handle_call({pin_init_output, Pin, Owner}, _From, State) ->
    %% @todo should refactor this so non_exclusive_pin_init code is
    %% reused.
    pin_init(Pin, output, Owner, State);
handle_call({pin_release, Pin}, _From, State) ->
    case lists:keytake(Pin, #pin_info.number, State#state.pins) of
        {value, #pin_info{}, NewPins} ->
            Reply = ?GPIO_MODULE:release(Pin),
            {reply, Reply, State#state{pins=NewPins}};
        false ->
            Reply = {error, release_of_uninitialised_pin},
            {reply, Reply, State}
    end;
handle_call({pin_write, Pin, Value, Requester},
            _From,
            State) ->
    case lists:keyfind(Pin, #pin_info.number, State#state.pins) of
        #pin_info{direction=output,
                  owner=Owner} when Owner == none;
                                    Owner == Requester ->
            Reply = ?GPIO_MODULE:write(Pin, Value),
            {reply, Reply, State};
        #pin_info{} ->
            Reply = {error, not_an_output_pin},
            {reply, Reply, State};
        false ->
            Reply = {error, uninitialised_pin},
            {reply, Reply, State}
    end;
handle_call({pin_read, Pin},
            _From,
            #state{pins=Pins}=State) ->
    case lists:keyfind(Pin, #pin_info.number, Pins) of
        #pin_info{direction=input} ->
            Reply = ?GPIO_MODULE:read(Pin),
            {reply, Reply, State};
        #pin_info{} ->
            Reply = {error, not_an_input_pin},
            {reply, Reply, State};
        false ->
            Reply = {error, uninitialised_pin},
            {reply, Reply, State}
    end;
handle_call({pin_set_int, Pin, Condition, Requester},
            _From,
            #state{pins=Pins}=State) ->
    case lists:keyfind(Pin, #pin_info.number, Pins) of
        #pin_info{direction=input,interrupts=none}=PinInfo->
            case ?GPIO_MODULE:set_int(Pin,Condition) of
                ok ->
                    NewPinInfo = PinInfo#pin_info{interrupts={Condition, [Requester]}},
                    NewPins    = lists:keystore(Pin, #pin_info.number, Pins, NewPinInfo),
                    {reply, ok, State#state{pins=NewPins}};
                Error ->
                    {reply, Error, State}
            end;
        #pin_info{direction=input, interrupts={Condition,Pids}}=PinInfo ->
            NewPinInfo = PinInfo#pin_info{interrupts={Condition,[Requester|Pids]}},
            NewPins    = lists:keystore(Pin, #pin_info.number, Pins, NewPinInfo),
            {reply, ok, State#state{pins=NewPins}};
        #pin_info{direction=input} ->
            Reply = {error, interrupt_condition_not_compatible_with_existing},
            {reply, Reply, State};
        #pin_info{} ->
            Reply = {error, not_an_input_pin},
            {reply, Reply, State};
        false ->
            Reply = {error, uninitialised_pin},
            {reply, Reply, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

pin_init(Pin, Direction, Owner, State) ->
    case lists:keyfind(Pin, #pin_info.number, State#state.pins) of
        #pin_info{} ->
            Reply = {error, pin_already_initialised},
            {reply, Reply, State};
        false ->
            try ?GPIO_MODULE:init(Pin, Direction) of
                {ok, _Pid} ->
                    PinInfo = #pin_info{number=Pin,
                                        direction=Direction,
                                        owner=Owner},
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

handle_info({gpio_interrupt, Pin, Condition}=Msg, #state{pins=Pins}=State) ->
    io:format("gpio:handle_info(~w, ~w)~n",[Msg, State]),
    case lists:keyfind(Pin, #pin_info.number, Pins) of
        #pin_info{direction=input,
                  interrupts={Condition, Listeners}} ->
            [ L ! Msg || L <- Listeners ],
            {noreply, State};
        #pin_info{} ->
            %% @todo must log this error thing in a sensible manner
            {noreply, State};
        false ->
            %% @todo must log this error thing in a sensible manner
            {noreply, State}
    end;
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

