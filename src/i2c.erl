%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This is the implementation of the I2C interface module.
%%% There is one process for each i2c device. Each process is linked to the supervisor
%%% of the i2c application.
%%% @end

-module(i2c).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([write/3, read/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(I2CLIBRARY, "priv/i2c_lib").

-type addr() :: integer().
-type data() :: tuple().
-type len() :: integer().
-type devname() :: string().
-type channel() :: atom().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the process with the channel name and Initialize the devname device.
%% You can identify the device by a channel name. Each channel drive a devname device.
%% @end
-spec(start_link({channel(), devname()}) -> {ok, pid()} | {error, reason}).
start_link({Channel, Devname}) ->
    gen_server:start_link({local, Channel}, ?MODULE, Devname, []).

%% @doc
%% Stop the process channel and release it.
%% @end
stop(Channel) ->
    gen_server:cast(Channel, stop).

%% @doc 
%% Write data into an i2c slave device.
%% @end
-spec(write(channel(), addr(), data()) -> ok | {error, reason}).
write(Channel, Addr, Data) ->
    gen_server:call(Channel, {call, write, Addr, Data}).

%% @doc 
%% Read data from an i2c slave device.
%% @end
-spec(read(channel(), addr(), len()) -> {data()} | {error, reason}).
read(Channel, Addr, Len) ->
    gen_server:call(Channel, {call, read, Addr, Len}).

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
init(Devname) ->
    Port = open_port({spawn, ?I2CLIBRARY}, [{packet, 2}, binary]),
    i2c_init(Port, Devname),
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
handle_call({call, write, Addr, Data}, _From, State) ->
    Len = tuple_size(Data),
    case port_lib:sync_call_to_port(State, {i2c_write, Addr, Data, Len}) < 0 of
	true ->
	    Reply = {error, i2c_write_error};
	false ->
	    Reply = ok
    end,
    {reply, Reply, State};

handle_call({call, read, Addr, Len}, _From, State) ->
    Res = port_lib:sync_call_to_port(State, {i2c_read, Addr, Len}),
    case Res of
	-1 ->
	    Reply = {error, i2c_read_error};
	Reply ->
	    ok
    end,
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

%% @doc Initialize the i2c devname device.
%% @end
i2c_init(Port, Devname) ->
    case port_lib:sync_call_to_port(Port, {i2c_init, Devname}) < 0 of
	true ->
	    exit({error, i2c_initialization_error});
	false ->
	    ok
    end.
