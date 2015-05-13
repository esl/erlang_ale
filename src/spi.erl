%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd | 2015, Frank Hunleth
%%% @doc
%%% This is the implementation of the SPI interface module.
%%% @end

-module(spi).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, stop/1]).
-export([transfer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type data() :: binary().
-type devname() :: string().
-type server_ref() :: atom() | {atom(), atom()} | pid().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the process and initialize the device.
%% @end
-spec(start_link(term(), devname(), list()) -> {ok, pid()} | {error, reason}).
start_link(ServerName, Devname, SpiOptions) ->
    gen_server:start_link(ServerName, ?MODULE, {Devname, SpiOptions}, []).

-spec(start_link(devname(), list()) -> {ok, pid()} | {error, reason}).
start_link(Devname, SpiOptions) ->
    gen_server:start_link(?MODULE, {Devname, SpiOptions}, []).

%% @doc
%% Stop the process channel and release it.
%% @end
-spec(stop(server_ref()) -> ok).
stop(ServerRef) ->
    gen_server:cast(ServerRef, stop).

%% @doc
%% Transfer data trough the SPI bus.
%% @end
-spec(transfer(server_ref(), data()) -> data() | {error, reason}).
transfer(ServerRef, Data) ->
    gen_server:call(ServerRef, {transfer, Data}).

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
init({Devname, SpiOptions}) ->
    Mode = keyword_get(SpiOptions, mode, 0),
    BitsPerWord = keyword_get(SpiOptions, bits_per_word, 8),
    SpeedHz = keyword_get(SpiOptions, speed_hz, 1000000),
    DelayUs = keyword_get(SpiOptions, delay_us, 10),

    Port = ale_util:open_port(["spi",
                               "/dev/" ++ Devname,
                               integer_to_list(Mode),
                               integer_to_list(BitsPerWord),
                               integer_to_list(SpeedHz),
                               integer_to_list(DelayUs)]),
    {ok, Port}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({transfer, Data}, _From, State) ->
    Reply = call_port(State, transfer, Data),
    {reply, Reply, State}.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
keyword_get(Keywords, Key, Default) ->
    case lists:keyfind(Key, 1, Keywords) of
        {Key, Value} -> Value;
        false -> Default
    end.


call_port(Port, Command, Args) ->
    Message = {Command, Args},
    erlang:send(Port, {self(), {command, term_to_binary(Message)}}),
    receive
        {_, {data, Response}} -> binary_to_term(Response)
    end.
