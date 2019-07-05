%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd | 2015, Frank Hunleth
%%% @doc
%%% This is the implementation of the I2C interface module.
%%% @end

-module(i2c).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3, start_link/4, stop/1]).
-export([write/2, read/2, write_read/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type addr() :: integer(). %% fix to be 2-127
-type data() :: binary().
-type len() :: integer().
-type devname() :: string().
-type server_ref() :: atom() | {atom(), atom()} | pid().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the process with the channel name and Initialize the devname device.
%% You can identify the device by a channel name. Each channel drive a devname device.
%% @end
-spec(start_link(devname(), addr()) -> {ok, pid()} | {error, term()}).
start_link(DevName, Address) ->
    start_link(DevName, Address, 0).

-spec start_link(devname() | tuple(), addr() | devname(), non_neg_integer())
                -> {ok, pid()} | {error, term()}.
start_link(DevName, Address, MaxBlockSize) when is_list(DevName) ->
    gen_server:start_link(?MODULE, {DevName, Address, MaxBlockSize}, []);
start_link(ServerName, DevName, Address) when is_tuple(ServerName), is_list(DevName) ->
    start_link(ServerName, DevName, Address, 0).

start_link(ServerName, DevName, Address, MaxBlockSize) ->
    gen_server:start_link(ServerName, ?MODULE, {DevName, Address, MaxBlockSize}, []).


%% @doc
%% Stop the process channel and release it.
%% @end
-spec(stop(server_ref()) -> ok).
stop(ServerRef) ->
    gen_server:cast(ServerRef, stop).

%% @doc
%% Write data into an i2c slave device.
%% @end
-spec(write(server_ref(), data()) -> ok | {error, term()}).
write(ServerRef, Data) ->
    gen_server:call(ServerRef, {write, Data}).

%% @doc
%% Read data from an i2c slave device.
%% @end
-spec(read(server_ref(), len()) -> data() | {error, term()}).
read(ServerRef, Len) ->
    gen_server:call(ServerRef, {read, Len}).

-spec(write_read(server_ref(), data(), len()) -> data() | {error, term()}).
write_read(ServerRef, Data, Len) ->
    gen_server:call(ServerRef, {wrrd, Data, Len}).

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
%%                     {stop, term()}
%% @end
%%--------------------------------------------------------------------
init({Devname, Address, MaxBlockSize}) ->
    Port = ale_util:open_port(["i2c",
                               "/dev/" ++ Devname,
                               integer_to_list(Address),
                               integer_to_list(MaxBlockSize)]),
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
%%                                   {stop, term(), Reply, State} |
%%                                   {stop, term(), State}
%% @end
%%--------------------------------------------------------------------
handle_call({write, Data}, _From, State) ->
    Reply = call_port(State, write, Data),
    {reply, Reply, State};

handle_call({read, Len}, _From, State) ->
    Reply = call_port(State, read, Len),
    {reply, Reply, State};

handle_call({wrrd, Data, Len}, _From, State) ->
    Reply = call_port(State, wrrd, {Data, Len}),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, term(), State}
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
%%                                   {stop, term(), State}
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

call_port(Port, Command, Args) ->
    Message = {Command, Args},
    erlang:send(Port, {self(), {command, term_to_binary(Message)}}),
    receive
        {_, {data, Response}} -> binary_to_term(Response)
    end.
