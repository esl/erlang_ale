%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This is the supervisor module for Erlang/ALE.
%% @end

-module(erlang_ale_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%===================================================================
%% Includes
%%===================================================================
-include("ale_type_def.hrl").
-include("ale_common.hrl").

%%===================================================================
%% Defines
%%===================================================================
-define(SERVER, ?MODULE).

%%===================================================================
%% @doc
%% Start the supervisor.
%% @end
-spec start_link() -> {ok, pid()} | {shutdown, term()} | {error, term()} | ignore | term().
%%===================================================================
start_link() ->
	case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
		{ok, SupPidT} ->
			{ok, SupPidT};
		{error, {already_started, SupPidT}} ->
			{ok, SupPidT};
		ER->
			ER
	end.

%%===================================================================
%% @doc
%% Init the supervisor.
%% @end
%%===================================================================
init([]) ->
	Procs = [],
	MaxRestart = 10,
	MaxTime = 3600,
	{ok, {{one_for_one, MaxRestart, MaxTime}, Procs}}.


%%===================================================================
%% Internal functions.
%%===================================================================
=======
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.
