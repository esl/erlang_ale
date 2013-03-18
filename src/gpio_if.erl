%% @doc This is the real implementation of the gpio interface module.
%% It follows the API of the sim_gpio module, so there will be one
%% process per pin.
%% The main difference is that this module is talking to a c-node that
%% does the low level stuff.
%% @end

-module(gpio_if).

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
          interrupt = no_interrupt :: pin_interrupt(),
          nodename                 :: atom(),
          pending   = []           :: [term()] 
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
    NodeName = nodename(),
    State = #state{pin=Pin,
                   direction=Direction,
                   nodename=NodeName},
    ok = sync_call_to_node(State, {init, Pin, Direction}),
    {ok, State}.


handle_call(release, _From, State) ->
    {stop, normal, ok, State};
handle_call({write, Value}, From, #state{pin=Pin,
                                          direction=output,
                                          pending=Pending}=State) ->
    call_to_node(State, From, {write, Pin, Value}),
    NewPending = [From | Pending],
    {noreply,  State#state{pending=NewPending}};
handle_call({write, _Value}, _From, #state{direction=input}=State) ->
    %% @todo: check with Ömer what the behaviour should be here
    Reply = {error, writing_to_input_pin},
    {reply, Reply, State};
handle_call(read, From, #state{direction=input,
                               pin=Pin,
                               pending=Pending}=State) ->
    call_to_node(State, From, {read, Pin}),
    NewPending = [From | Pending ],
    {noreply, State#state{pending=NewPending}};
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
    {reply, Reply, State}.
            
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({To, Msg}, #state{pending=Pending}=State) ->
    %% @todo: should we do something if To is not in Pending list?
    %% io:format("Got a reply to a pending message {~p, ~p}", [To, Msg]),
    NewPending = lists:delete(To, Pending),
    gen_server:reply(To, Msg),
    {noreply, State#state{pending=NewPending}}.

terminate(_Reason, #state{pin=Pin}=State) ->
    %% the gproc entry is automatically removed.
    ok = sync_call_to_node(State, {release, Pin}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

register(Pin) ->
    gproc:reg({n, l, pname(Pin)}).

pname(Pin) ->
    {gpio_pin, Pin}.


%% @doc value_change(Old, New)
value_change(0, 1) ->
    raising;
value_change(1, 0) ->
    falling;
value_change(_, _) ->
    no_change.

%% @doc the bare bone sending of a message to the cnode
send_to_cnode(#state{nodename=NodeName}, Msg) ->
    {any, NodeName} ! Msg.


%% @doc this is a synchronous call as it waits for a respose from the cnode.

sync_call_to_node(State, Msg) ->
    Ref = make_ref(),
    send_to_cnode(State, {call, self(), Ref,  Msg}),
    receive
        {Ref, Result} ->
            Result
    end.

%% the response to this call has to be handled in the handle_info function.
call_to_node(State, From, Msg) ->
    send_to_cnode(State, {call, self(), From, Msg}).

nodename() ->
    {ok, Hostname} = inet:gethostname(),
    list_to_atom("c1@" ++ Hostname).
