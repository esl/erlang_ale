%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This module allow to use erlang/ALE to send and receive data through i2c bus.
%%% @end

-module(i2c).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([i2c_init/1, i2c_init_name/2, i2c_write/4, i2c_read/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define (I2CLIBRARY, "../priv/i2c_lib").

-type addr() :: integer().
-type data() :: tuple().
-type len() :: integer().
-type devname() :: string().
-type channel() :: atom().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Channel) ->
    gen_server:start_link({local, Channel}, ?MODULE, [], []).

stop(Channel) ->
    gen_server:cast(Channel, stop).

%% @doc Initialize the i2c bus.
%% @end
-spec(i2c_init(channel()) -> ok | {error, error_type}).
i2c_init(Channel) ->
    gen_server:call(Channel, {call, i2c_init}).

%% @doc Initialize the i2c devname device.
%% @end
-spec(i2c_init_name(channel(), devname()) -> ok | {error, error_type}).
i2c_init_name(Channel, Devname) ->
    gen_server:call(Channel, {call, i2c_init_name, Devname}).

%% @doc write data into an i2c slave device.
%% @end
-spec(i2c_write(channel(), addr(), data(), len()) -> ok | {error, error_type}).
i2c_write(Channel, Addr, Data, Len) ->
    gen_server:call(Channel, {call, i2c_write, Addr, Data, Len}).

%% @doc read data from an i2c slave device.
%% @end
-spec(i2c_read(channel(), addr(), len()) -> {data()} | {error, error_type}).
i2c_read(Channel, Addr, Len) ->
    gen_server:call(Channel, {call, i2c_read, Addr, Len}).

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
init([]) ->
    Port = open_port({spawn, ?I2CLIBRARY}, [{packet, 2}, binary]),
%    i2c_init(),
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
handle_call({call, i2c_init}, _From, State) ->
    case port_lib:sync_call_to_port(State, {i2c_init}) < 0 of
	true ->
	    Reply = {error, i2c_initialization_error};
	false ->
	    Reply = ok
    end,
    {reply, Reply, State};

handle_call({call, i2c_init_name, Devname}, _From, State) ->
    case port_lib:sync_call_to_port(State, {i2c_init_name, Devname}) < 0 of
	true ->
	    Reply = {error, i2c_initialization_error};
	false ->
	    Reply = ok
    end,
    {reply, Reply, State};

handle_call({call, i2c_write, Addr, Data, Len}, _From, State) ->
    case port_lib:sync_call_to_port(State, {i2c_write, Addr, Data, Len}) < 0 of
	true ->
	    Reply = {error, i2c_write_error};
	false ->
	    Reply = ok
    end,
    {reply, Reply, State};

handle_call({call, i2c_read, Addr, Len}, _From, State) ->
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
handle_info({State, {data, Data}}, State) ->
    io:format("Message received: ~p~n",[binary_to_term(Data)]),
    {noreply, State};

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
