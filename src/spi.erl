%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This is the implementation of the SPI interface module.
%%% There is one process for each spi device. Each process is linked to the supervisor
%%% of the spi application.
%%% @end

-module(spi).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([config/5, transfer/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define (SPILIBRARY, "priv/spi_lib").

-type mode() :: integer().
-type data() :: tuple().
-type bits() :: integer().
-type delay() :: integer().
-type speed() :: integer().
-type len() :: integer().
-type devname() :: string().
-type channel() :: atom().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Starts the process with the channel name and Initialize the devname device.
%% You can identify the device by a channel name. Each cannel drive a devname device.
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
%% Configure the SPI device.
%% @end
-spec(config(channel(), mode(), bits(), speed(), delay()) -> ok | {error, reason}).
config(Channel, Mode, Bits, Speed, Delay) ->
    gen_server:call(Channel, {call, config, Mode, Bits, Speed, Delay}).

%% @doc
%% Transfer data trough the SPI bus.
%% @end
-spec(transfer(channel(), data(), len()) -> {data()} | {error, reason}).
transfer(Channel, Data, Len) ->
    gen_server:call(Channel, {call, transfer, Data, Len}).

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
    Port = open_port({spawn, ?SPILIBRARY}, [{packet, 2}, binary]),
    spi_init(Port, Devname),
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
handle_call({call, config, Mode, Bits, Speed, Delay}, _From, State) ->
    case port_lib:sync_call_to_port(State, {spi_config, Mode, Bits, Speed, Delay}) < 0 of
	true ->
	    Reply = {error, spi_configuration_error};
	false ->
	    Reply = ok
    end,
    {reply, Reply, State};

handle_call({call, transfer, Data, Len}, _From, State) ->
    Res = port_lib:sync_call_to_port(State, {spi_transfer, Data, Len}),
    case Res of
	-1 ->
	    Reply = {error, spi_transfer_error};
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

%% @doc Initialize the SPI devname device.
%% @end
spi_init(Port, Devname) ->
    case port_lib:sync_call_to_port(Port, {spi_init, Devname}) < 0 of
	true ->
	    exit({error, spi_initialization_error});
	false ->
	    ok
    end.
