%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This module allow to use erlang/ALE to generate PWM signals.
%%% @end

-module(pwm).

-export([load_nif/0, init/0, value/1, release/0]).

-onload([load_nif/0]).

%% @doc Load the PWM C library.
%% @end
-spec(load_nif() -> ok | {error, error_type}).

load_nif() ->
    ok = erlang:load_nif("./pwm_nif", 0).

%% @doc Initialise the PWM peripheral.
%% @end
-spec(init() -> ok | {error,pwm_initialization_error}).

init() ->
    exit(nif_library_not_loaded).

%% @doc Set PWM value.
%% @end
-type val() :: 0..1024.
-spec(value(val()) -> ok).

value(_Val) ->
    exit(nif_library_not_loaded).

%% @doc Releases the PWM peripheral and unmaps the memory.
%% @end
-spec(release() -> ok).

release() ->
    exit(nif_library_not_loaded).
