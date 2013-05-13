-module(rebar_compiledeps_plugin).
-export([pre_compile/2]).

pre_compile(_, _) ->
    Cwd = rebar_utils:get_cwd(),
    io:format("rebar cwd: ~p~n", [Cwd]),
    case lists:suffix("pihwd", Cwd) of
        true ->
            Opts = [{cwd, Cwd}],
            case filelib:is_regular(filename:join([Cwd, "Makefile"])) of
                true ->
                    rebar_utils:sh("make [OPTIONS]", Opts);
                false ->
                    rebar_utils:sh("./configure && make [OPTIONS]", Opts)
            end;
        false ->
            ok
    end,
    ok.
