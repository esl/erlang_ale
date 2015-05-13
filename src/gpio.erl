%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd | 2015, Frank Hunleth
%%% @doc
%%% This is the implementation of the GPIO interface module.
%%% @end

-module(gpio).

-behaviour(gen_server).

%% API
-export([start_link/2,
         start_link/3,
         stop/1,
         write/2,
         read/1,
         set_int/2,
         register_int/1,
         register_int/2,
         unregister_int/1,
         unregister_int/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(REPLY, 0).
-define(NOTIFICATION, 1).

-type pin() :: non_neg_integer().
-type pin_direction() :: 'input' | 'output'.
-type pin_state() :: 0 | 1.
-type interrupt_condition() :: 'enabled' | 'summarize' | 'none' | 'rising' | 'falling' | 'both'.
-type server_ref() :: atom() | {atom(), atom()} | pid().

-export_type([interrupt_condition/0]).

-record(state,
        { pin               :: pos_integer(),
          pids = []         :: [pid()],
          port              :: port()
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts a process to handle a GPIO.
%% @end
-spec start_link(term(), pin(), pin_direction()) ->
                    {'ok', pid()} | 'ignore' | {'error', term()}.
start_link(ServerName, Pin, Direction) ->
  gen_server:start_link(ServerName, ?MODULE, {Pin, Direction}, []).

-spec(start_link(pin(), pin_direction()) -> {ok, pid()} | {error, reason}).
start_link(Pin, Direction) ->
  gen_server:start_link(?MODULE, {Pin, Direction}, []).

%% @doc
%% Stop the process channel and release it.
%% @end
-spec(stop(server_ref()) -> ok).
stop(ServerRef) ->
    gen_server:cast(ServerRef, stop).

%% @doc write/2 sets an output pin to the value given.
%% @end
-spec write(server_ref(), pin_state()) -> 'ok' | {'error', 'writing_to_input_pin'}.
write(ServerRef, Value) ->
  gen_server:call(ServerRef, {write, Value}).

%% @doc read/1 returns the value of an input pin.
%% @end
-spec read(server_ref()) -> pin_state() | {'error', 'reading_from_output_pin'}.
read(ServerRef) ->
  gen_server:call(ServerRef, read).

%% @doc set_int/2 configures how interrupts are notified.
%%
%% Valid modes include:
%%    'none'      Interrupt norification is disabled
%%    'falling'   Only falling transitions are notified
%%    'rising'    Only rising transitions are notified
%%    'both'      Both rising and falling transitions are notified
%%    'summarize' If interrupts come too quickly, coallesce transitions
%%
%% See register_int/1 and register_int/2 for enabling event generation.
%% @end
-spec set_int(server_ref(), interrupt_condition()) -> 'ok' | {'error', term()}.
set_int(ServerRef, Condition) when Condition == enabled;
                             Condition == summerize;
                             Condition == both;
                             Condition == rising;
                             Condition == falling;
                             Condition == none ->
  gen_server:call(ServerRef, {set_int, Condition}).

%% @doc register_int/2 registers a process to receive interrupt notifications.
%%
%% The requesting process will be sent a message with the structure
%% <code>{gpio_interrupt, Pin, Condition}</code> when the interrupt triggers.
%% @end
-spec register_int(server_ref(), pid() | atom()) -> 'ok' | {'error', term()}.
register_int(ServerRef, Dest) ->
  gen_server:call(ServerRef, {register_int, Dest}).

%% @doc register_int/2 registers the caller to receive interrupt notifications.
%%
%% The requesting process will be sent a message with the structure
%% <code>{gpio_interrupt, Pin, Condition}</code> when the interrupt triggers.
%% @end
-spec register_int(server_ref()) -> 'ok' | {'error', term()}.
register_int(ServerRef) ->
  register_int(ServerRef, self()).

%% @doc unregister_int/2 unregisters a process from receiving interrupt notifications.
%% @end
unregister_int(ServerRef) ->
  gen_server:call(ServerRef, {unregister_int, self()}).

%% @doc unregister_int/2 unregisters the caller from receiving interrupt notifications.
%% @end
unregister_int(ServerRef, Pid) ->
  gen_server:call(ServerRef, {unregister_int, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init({Pin, Direction}) ->
    Port = ale_util:open_port(["gpio",
                               integer_to_list(Pin),
                               atom_to_list(Direction)]),
    {ok, #state{pin=Pin, port=Port}}.

handle_call({write, Value}, _From, #state{port=Port}=State) ->
    Reply = call_port(Port, write, Value),
    {reply, Reply, State};
handle_call(read, _From, #state{port=Port}=State) ->
    Reply = call_port(Port, read, []),
    {reply, Reply, State};
handle_call({set_int, Condition}, _From, #state{port=Port}=State) ->
    call_port(Port, set_int, Condition),
    {reply, ok, State};
handle_call({register_int, Pid}, _From,
            #state{pids=Pids}=State) ->
    link(Pid),
    NewPids = [Pid|Pids],
    {reply, ok, State#state{pids=NewPids}};
handle_call({unregister_int, Pid}, _From,
            #state{pids=Pids}=State) ->
    NewPids = lists:delete(Pid, Pids),
    {reply, ok, State#state{pids=NewPids}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({Port, {data, <<?NOTIFICATION, Msg/binary>>}},
            #state{port=Port, pids=Pids}=State) ->
    Notif = binary_to_term(Msg),
    [ Pid ! Notif || Pid <- Pids ],
    {noreply, State};
handle_info({'EXIT', DeadPid, _Reason},     % a listener died
	    #state{pids=Pids}=State) ->
    NewPids = [ Pid || Pid <- Pids, Pid /= DeadPid ],
    {noreply, State#state{pids=NewPids}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

call_port(Port, Command, Args) ->
    Message = {Command, Args},
    erlang:send(Port, {self(), {command, term_to_binary(Message)}}),
    receive
        {Port, {data, <<?REPLY, Response/binary>>}} -> binary_to_term(Response)
    end.
