%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This is the supervisor of the GPIO application.
%%% @end

-module(gpio_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type pin() :: 0..63.
-type pin_direction() :: 'input' | 'output'.
-type chnlist() :: list({pin(), pin_direction()}).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Starts and initialize a list of process. Each one is identified by a
%% pin number.
%% @end
-spec(start_link(chnlist()) -> {ok, pid()} | {error, reason}).
start_link(Chnlist) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Chnlist).

%% @doc
%% Stop the GPIO supervisor.
%% @end
stop() ->
    application:stop(gproc),
    exit(whereis(?MODULE), shutdown).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Chnlist) ->
    application:start(gproc),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, child_list(Chnlist)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% GPIO process specification
child_list([H | T]) ->
%%    Restart = permanent,
    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    Child = {H, {gpio, start_link, [H]},
	      Restart, Shutdown, Type, [gpio]},
    [Child | child_list(T)];
child_list([]) ->
    [].
