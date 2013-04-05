%% @doc This is the real implementation of the gpio interface module.
%% It follows the API of the sim_gpio module, so there will be one
%% process per pin.
%% The main difference is that this module is talking to a c-node that
%% does the low level stuff.
%% @end
 
-module(gpio).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% SW API
-export([init/2,
         release/1,
         write/2,
         read/1,
         set_int/2]).

-export([from_port/2]).

%% %% HW manipulation
%% -export([set_value/2, %% this is for input pins
%%          get_value/1 %% checking output pins
%%          ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-type pin_direction() :: 'input' | 'output'.

-type pin_state() :: 0 | 1.
-type interrupt_condition() :: 'none' | 'rising' | 'falling' | 'both'.
-type pin_interrupt() :: 'none' |
                         {interrupt_condition(), [pid()]}.

-export_type([interrupt_condition/0]).

-record(state,
        { pin               :: pos_integer(),
          direction = input :: pin_direction(),
          interrupt = none  :: pin_interrupt(),
          port              :: port(),
          pending   = []    :: [term()] 
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% @todo Add init for exclusive pins
init(Pin, Direction) ->
    case gproc:lookup_local_name(pname(Pin)) of
        undefined ->
            start_link(Pin, Direction);
        _Pid ->
            {error, pin_already_initialised}
    end.
            
release(Pin) ->
    call_existing(Pin, release).


write(Pin, Value) ->
    call_existing(Pin, {write, Value}).

read(Pin) ->
    call_existing(Pin, read).

set_int(Pin, Condition) when Condition == rising;
                             Condition == falling;
                             Condition == both;
                             Condition == none ->
    Requestor = self(),
    call_existing(Pin, {set_int, Condition, Requestor});
set_int(_Pin, _Condition) ->
    {error, wrong_condition}.

from_port(Pin, Msg) ->
    call_existing(Pin, {from_port, Msg}).


%% %% HW input simulation
%% set_value(Pin, Value) ->
%%     call_existing(Pin, {set_value, Value}).

%% %% check what an output pin has been set to.
%% get_value(Pin) ->
%%     call_existing(Pin, get_value).

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
    SharedLib = "gpio_port",
    ok = port_lib:load_driver(SharedLib),
    register(Pin),
    Port = port_lib:open_port(SharedLib),
    State = #state{pin=Pin,
                   direction=Direction,
                   port=Port},
    ok = port_lib:sync_call_to_port(Port, {init, Pin, Direction}),
    {ok, State}.




handle_call(release, _From, State) ->
    {stop, normal, ok, State};
handle_call({write, Value}, From, #state{direction=output,
                                         pending=Pending,
                                         port=Port}=State) ->
    port_lib:call_to_port(Port, From, {write, Value}),
    NewPending = [From | Pending],
    {noreply,  State#state{pending=NewPending}};
handle_call({write, _Value}, _From, #state{direction=input}=State) ->
    %% @todo: check with Ömer what the behaviour should be here
    Reply = {error, writing_to_input_pin},
    {reply, Reply, State};
handle_call(read, From, #state{direction=input,
                               pin=Pin,
                               pending=Pending,
                               port=Port}=State) ->
    port_lib:call_to_port(Port, From, read),
    NewPending = [From | Pending ],
    {noreply, State#state{pending=NewPending}};
handle_call(read, _From, #state{direction=output}=State) ->
    Reply = {error, reading_from_output_pin},
    {reply, Reply, State};
handle_call({set_int, Condition, Requestor},
            From,
            #state{direction=input,
                   interrupt=no_interrupt,
                   pending=Pending,
                   port=Port}=State) ->
    port_lib:call_to_port(Port, From, {set_int, Condition}),
    NewPending = [From | Pending],
    {noreply, State#state{interrupt={Condition, Requestor},
                          pending=NewPending}};
handle_call({set_int, Condition, Requestor},
            _From,
            #state{direction=input,
                   interrupt={Condition,Pids}}=State) ->
    Reply = ok,
    Interrupt = {Condition, [Requestor|Pids]},
    {reply, Reply, State#state{interrupt=Interrupt}};
handle_call({set_int, Condition, Requestor},
            _From,
            #state{direction=input,
                   interrupt={_,_}}=State) ->
    Reply = {error, unable_to_change_interrupt},
    {reply, Reply, State};
handle_call({set_int, _Condition, _Requestor},
            _From,
            #state{direction=output}=State) ->
    Reply = {error, setting_interrupt_on_output_pin},
    {reply, Reply, State};
handle_call({from_port, {gpio_interrupt, Condition}=Msg},
            _From,
            #state{interrupt={Condition, Pids}}=State) ->
    [ Pid ! Msg || Pid <- Pids ],
    Reply = ok,
    {reply, Reply, State};
handle_call({from_port, {port_reply, To, Msg}},
            _From,
           #state{pending=Pending}=State) ->
    %% @todo: should we do something if To is not in Pending list?
    NewPending = lists:delete(To, Pending),
    gen_server:reply(To, Msg),
    {reply, ok, State#state{pending=NewPending}}.

            
handle_cast(_Msg, State) ->
    {noreply, State}.


%% In order to be able to test using a mocking library we call
%% the from_port/2 function for all messages from the port. 
handle_info({Port, {data, Msg}},
            #state{port=Port, pin=Pin}=State) ->
    apply_after(0, ?MODULE, from_port, [Pin, Msg]),
    {noreply, State}.

terminate(_Reason, #state{port=Port}=State) ->
    %% the gproc entry is automatically removed.
    ok = port_lib:sync_call_to_port(Port, release).

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






    




apply_after(Time, M, F, Args) ->
    Ref = make_ref(),
    Self = self(),
    Pid = spawn( fun() ->
                         receive
                             Ref ->
                                 apply(M, F, Args)
                         end
                 end ),
    erlang:send_after(Time, Pid, Ref), 
    ok.
