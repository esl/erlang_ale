-module(port_lib).

-export([load_driver/1,
         open_port/1,
         sync_call_to_port/2,
         call_to_port/3]).


open_port(SharedLib) ->
    erlang:open_port({spawn,SharedLib}, []).


send_to_port(Owner, Port, Msg) ->
    Port ! {Owner, {command, Msg}}.

sync_call_to_port(Port, Msg) ->
    send_to_port(self(), Port, Msg),
    receive
        {Port, {data, Result}} ->
            Result
    end.

%% @doc the response to this call has to be handled in the handle_info
%% function. The From parameter is there to figure out which of the
%% pending requests that the response belongs to.
call_to_port(Port, From, Msg) ->
    send_to_port(self(), Port, {call, From, Msg}).


load_driver(SharedLib) ->
    case erl_ddll:load_driver(".", SharedLib) of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end.
