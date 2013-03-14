-module(sim_gpio).

-behaviour(gen_server).


%% API
-export([start_link/2]).
%% SW API
-export([init/2,
         release/1,
         write/2,
         read/1,
         set_int/2]).

%% HW manipulation
-export([set_value/2, %% this is for input pins
         get_value/1 %% checking output pins
         ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-type pin_direction() :: 'input' | 'output'.

-type pin_state() :: 0 | 1.
-type interrupt_condition() :: 'raising' | 'falling' | 'both'.
-type pin_interrupt() :: 'no_interrupt' |
                         {interrupt_condition(), pid()}.

-export_type([interrupt_condition/0]).

-record(state,
        { pin                      :: pos_integer(),
          direction = input        :: pin_direction(),
          value     = 0            :: pin_state(),
          interrupt = no_interrupt :: pin_interrupt()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% API
init(Pin, Direction) ->
    start_link(Pin, Direction).

release(Pin) ->
    call_existing(Pin, release).


write(Pin, Value) ->
    call_existing(Pin, {write, Value}).

read(Pin) ->
    call_existing(Pin, read).

set_int(Pin, Condition) ->
    Requestor = self(),
    call_existing(Pin, {set_int, Condition, Requestor}).

%% HW input simulation
set_value(Pin, Value) ->
    call_existing(Pin, {set_value, Value}).

%% check what an output pin has been set to.
get_value(Pin) ->
    call_existing(Pin, get_value).

call_existing(Pin, Msg) ->
    case gproc:lookup_local_name(pname(Pin)) of
        undefined ->
            {error, pin_not_present};
        Pid ->
            gen_server:call(Pid, Msg)
    end.


start_link(Pin, Direction) ->
    gen_server:start_link(?MODULE, [Pin, Direction], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Pin, Direction]) ->
    register(Pin),
    {ok, #state{pin=Pin,
                direction=Direction}}.

handle_call(release, _From, State) ->
    {stop, normal, ok, State};
handle_call({write, Value}, _From, #state{direction=output}=State) ->
    Reply = ok,
    {reply, Reply, State#state{value=Value}};
handle_call({write, _Value}, _From, #state{direction=input}=State) ->
    %% @todo: check with Ömer what the behaviour should be here
    Reply = {error, writing_to_input_pin},
    {reply, Reply, State};
handle_call(read, _From, #state{direction=input,
                                value=Value}=State) ->
    Reply = Value,
    {reply, Reply, State};
handle_call(read, _From, #state{direction=output}=State) ->
    Reply = {error, reading_from_output_pin},
    {reply, Reply, State};
handle_call({set_int, Condition, Requestor},
            _From,
            #state{direction=input,
                   interrupt=no_interrupt}=State) ->
    Reply = ok,
    {reply, Reply, State#state{interrupt={Condition, Requestor}}};
handle_call({set_int, Condition, Requestor},
            _From,
            #state{direction=input,
                   interrupt={_,_}}=State) ->
    %% @todo check with Omer that it is okay to overwrite the
    %% interrupt or if some other error behaviour should be used. 
    Reply = ok,
    {reply, Reply, State#state{interrupt={Condition, Requestor}}};
handle_call({set_int, _Condition, _Requestor},
            _From,
            #state{direction=output}=State) ->
    Reply = {error, setting_interrupt_on_output_pin},
    {reply, Reply, State};
handle_call({set_value, _}, _From, #state{direction=output}=State) ->
    Reply = {error, set_value_on_output_pin},
    {reply, Reply, State};
handle_call({set_value, Value},
            _From,
            #state{direction=input,
                  interrupt=no_interrupt}=State) ->
    Reply = ok,
    {reply, Reply, State#state{value=Value}};
%% set_value with the existing value does nothing.
handle_call({set_value, Value},
            _From,
            #state{direction=input,
                   value=Value}=State) ->
    {reply, ok, State};
%% now we know that the value is changing and when any change is
%% desired we simply trigger the interrupt.
handle_call({set_value, Value},
            _From,
            #state{direction=input,
                   value=OldValue,
                   interrupt={both, Requestor}}=State) ->
    case value_change(OldValue, Value) of
        no_change ->
            io:format("no value change~n"),
            ok;
        _ ->
            Requestor ! {gpio_interrupt, State#state.pin, both}
    end,
    {reply, ok, State#state{value=Value}};
handle_call({set_value, Value},
            _From,
            #state{direction=input,
                   value=OldValue,
                   interrupt={Condition, Requestor}}=State) ->
    case value_change(OldValue, Value) of
        Condition ->
            Requestor ! {gpio_interrupt, State#state.pin, Condition},
            {reply, ok, State#state{value=Value}};
        _ ->
            {reply, ok, State#state{value=Value}}
    end;
handle_call(get_value,
            _From,
            #state{direction=output,
                   value=Value}=State) ->
    {reply, Value, State}.
            
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% nothing to do when the process dies the gproc entry is
    %% automatically removed.
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register(Pin) ->
    gproc:reg({n, l, pname(Pin)}).

pname(Pin) ->
    {sim_gpio_pin, Pin}.


%% @doc value_change(Old, New)
value_change(0, 1) ->
    raising;
value_change(1, 0) ->
    falling;
value_change(_, _) ->
    no_change.
