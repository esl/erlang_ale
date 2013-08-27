-module(port_lib).

-export([load_driver/1,
         open_port/1,
         sync_call_to_port/2,
         call_to_port/3,
         cast_to_port/2]).


open_port(SharedLib) ->
    erlang:open_port({spawn,SharedLib}, [{packet, 2}, binary]).


send_to_port(Owner, Port, Msg) ->
    %%    Bin = term_to_binary(Msg),
%%--    io:format("send_to_port: ~p~n", [Msg]),
      Port ! {Owner, {command, term_to_binary(Msg)}}.
%%    io:format("send_to_port: Bin=~p~n", [Bin]),
%%    erlang:port_command(Port, Bin).
    
sync_call_to_port(Port, Msg) ->

    send_to_port(self(), Port, Msg),
    receive
        {Port, {data, BinResult}} ->
            Result = binary_to_term(BinResult),
%%--            io:format("sync_call_to_port result: ~p~n", [Result]),
            Result
    end.

%% @doc the response to this call has to be handled in the handle_info
%% function. The From parameter is there to figure out which of the
%% pending requests that the response belongs to.
call_to_port(Port, From, Msg) ->
    send_to_port(self(), Port, {call, From, Msg}).

%% @doc used when no reply is needed from the port
cast_to_port(Port, Msg) ->
    send_to_port(self(), Port, {cast, Msg}).

load_driver(SharedLib) ->
    case erl_ddll:load_driver(".", SharedLib) of
        ok -> ok;
        {error, already_loaded} -> ok;
        _ -> exit({error, could_not_load_driver})
    end.
