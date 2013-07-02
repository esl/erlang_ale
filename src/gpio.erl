%% @doc This is the implementation of the gpio interface module.
%%
%% There is one process per GPIO pin. Each process opens a C-port that
%% manipulates the hardware.
%%
%% The one-process-per-pin design has been choosen since it allows to
%% attach each pin to the appropriate place in the supervision tree of
%% the entire application.  
%% @end
%% @copyright 2013 Erlang Solutions Limited

-module(gpio).

-behaviour(gen_server).

%% API
-export([start_link/2,
         release/1,
         write/2,
         read/1,
         set_int/2]).

-export([from_port/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-type pin() :: 0..63.
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

%% @doc starts a pin with the given direction.
%%
%% To simplify things we aways identify the process for a pin by the
%% pin number, so there is no need to remember the Pid of a pin
%% process spawned.
%% @todo Add init for exclusive pins
%% @end
-spec start_link(pin(), pin_direction()) ->
                    {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(Pin, Direction) ->
  case gproc:lookup_local_name(pname(Pin)) of
    undefined ->
      gen_server:start_link(?MODULE, [Pin, Direction], []);
    _Pid ->
      {error, pin_already_initialised}
  end.


%% @doc release/1 stops the pin process and frees resources on the
%% hardware.
%% @end
-spec release(pin()) -> 'ok'.
release(Pin) ->
  call_existing(Pin, release).

%% @doc write/2 sets an output pin to the value given.
%% @end
-spec write(pin(), pin_state()) -> 'ok' | {'error', 'writing_to_input_pin'}.
write(Pin, Value) ->
  call_existing(Pin, {write, Value}).

%% @doc read/1 returns the value of an input pin.
%% @end
-spec read(pin()) -> pin_state() | {'error', 'reading_from_output_pin'}.
read(Pin) ->
  call_existing(Pin, read).

%% @doc set_int/2 sets an interrupt on a pin with a given condition.
%% The requesting process will be sent a message with the structure
%% {gpio_interrupt, Condition} when the interrupt triggers.
%% More than one process can listen on an interrupt condition, but
%% only one interrupt condition can be set at a time.
%% @todo Specify the errors more precisely.
%% @end
-spec set_int(pin(), interrupt_condition()) -> 'ok' | {'error', Error}
                                                 when Error :: 'wrong_condition'
                                                             | term().
set_int(Pin, Condition) when Condition == rising;
                             Condition == falling;
                             Condition == both;
                             Condition == none ->
  Requestor = self(),
  call_existing(Pin, {set_int, Condition, Requestor});
set_int(_Pin, _Condition) ->
  {error, wrong_condition}.

%% @doc from_port/2 should not be called from the outside!
%% It is used to transform the messages from a port into a message
%% that is sent to the pin process using call instead of forcing us to
%% handle the messages in handle_info/2.
%% @end
from_port(Pin, Msg) ->
  call_existing(Pin, {from_port, Msg}).



call_existing(Pin, Msg) ->
  case gproc:lookup_local_name(pname(Pin)) of
    undefined ->
      {error, pin_not_present};
    Pid ->
      gen_server:call(Pid, Msg)
  end.



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Pin, Direction]) ->
  SharedLib = "priv/gpio_port",
  %%    ok = port_lib:load_driver(SharedLib),
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
                               pending=Pending,
                               port=Port}=State) ->
  port_lib:call_to_port(Port, From, {read}),
  NewPending = [From | Pending ],
  {noreply, State#state{pending=NewPending}};
handle_call(read, _From, #state{direction=output}=State) ->
  Reply = {error, reading_from_output_pin},
  {reply, Reply, State};
handle_call({set_int, Condition, Requestor},
            From,
            #state{direction=input,
                   interrupt=none,
                   pending=Pending,
                   port=Port}=State) ->
  port_lib:call_to_port(Port, From, {set_int, Condition}),
  NewPending = [From | Pending],
  {noreply, State#state{interrupt={Condition, [Requestor]},
                        pending=NewPending}};
handle_call({set_int, Condition, Requestor},
            _From,
            #state{direction=input,
                   interrupt={Condition,Pids}}=State) ->
  Reply = ok,
  Interrupt = {Condition, [Requestor|Pids]},
  {reply, Reply, State#state{interrupt=Interrupt}};
handle_call({set_int, _Condition, _Requestor},
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
handle_call({from_port, {gpio_interrupt, Condition}},
            _From,
            #state{pin=Pin,
                   interrupt={Condition, Pids}}=State) ->
  Msg = {gpio_interrupt, Pin, Condition},
  [ Pid ! Msg || Pid <- Pids ],
  Reply = ok,
  {reply, Reply, State};
handle_call({from_port, {port_reply, To, Msg}}=FullMsg,
            _From,
            #state{pending=Pending}=State) ->
  %% @todo: should we do something if To is not in Pending list?
  io:format("FullMsg=~p~n", [FullMsg]),
  NewPending = lists:delete(To, Pending),
  gen_server:reply(To, Msg),
  {reply, ok, State#state{pending=NewPending}}.


handle_cast(_Msg, State) ->
  {noreply, State}.


%% In order to be able to test using a mocking library we call
%% the from_port/2 function for all messages from the port. 
handle_info({Port, {data, Msg}},
            #state{port=Port, pin=Pin}=State) ->
  timer:apply_after(0, ?MODULE, from_port, [Pin, binary_to_term(Msg)]),
  {noreply, State}.

terminate(_Reason, #state{port=Port}=_State) ->
  %% the gproc entry is automatically removed.
  port_lib:cast_to_port(Port, release).

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
%% value_change(0, 1) ->
%%   raising;
%% value_change(1, 0) ->
%%   falling;
%% value_change(_, _) ->
%%   no_change.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
