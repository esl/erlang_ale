%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2015, Frank Hunleth
%%% @doc
%%% This file contains common code used throughout Erlang/ALE.
%%% @end

-module(ale_util).

%% API
-export([open_port/1
         ]).


-spec open_port([list()]) -> port().
open_port(Args) ->
    open_port({spawn_executable, code:priv_dir(erlang_ale) ++ "/erlang-ale"},
              [{packet, 2},
              binary,
              use_stdio,
              exit_status,
              {args, Args}]).

