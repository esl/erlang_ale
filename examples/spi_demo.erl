%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc Sends and receives data to/from MCP3002 A/D converter through the I2C bus.
%%% It reads the state of DIP Switch pins and turn on some led.
%%% @end

-module(spi_demo).

-export([init/0, mcp3002_read/1, run_demo/0, start/0, start/1, stop/0]).

-define(SPICHANNEL, spi1).
-define(SPIMODE, 0).
-define(SPIBPW, 8).
-define(SPISPEED, 1000000).
-define(SPIDELAY, 10).

init() ->
    spi_sup:start_link([{?SPICHANNEL, "/dev/spidev0.0"}]),   
    spi:config(?SPICHANNEL, ?SPIMODE, ?SPIBPW, ?SPISPEED, ?SPIDELAY).

run_demo() ->
    init(),
    start(),
    stop().

mcp3002_read(Channel) ->
    {_, Val1, Val2} = spi:transfer(?SPICHANNEL, {1, (2 + Channel) bsl 6, 0}, 3),
    ((Val1 band 31) bsl 6) + (Val2 bsr 2).

start() ->
    start(0).

start(X) when X < 10 ->
    io:format("Channel 0: ~p, Channel 1: ~p~n",[mcp3002_read(0), mcp3002_read(1)]),
    timer:sleep(1000),
    start(X+1);
start(_X) ->
    ok.

stop() ->
    spi:stop(?SPICHANNEL),
    spi_sup:stop(),
    bye.
