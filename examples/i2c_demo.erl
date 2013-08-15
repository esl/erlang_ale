%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc Sends and receives data to/from MCP23008 port expander through the I2C bus.
%%%      It reads the state of DIP Switch pins and turn on some led.
%%% @end

-module(i2c_demo).

-export([init/0, run_demo/0, start/0, start/1, stop/0]).

-define(ADDR, 16#20).
-define(IODIR, 16#0).
-define(GPIO, 16#9).

-define(I2C_CHANNEL, i2c1).

init() ->
    %% starts the supervisor
    i2c_sup:start_link([?I2C_CHANNEL]),
    %% initialize i2c bus
    i2c:i2c_init(?I2C_CHANNEL, "/dev/i2c-1"),
    %% set pin directions
    i2c:i2c_write(?I2C_CHANNEL, ?ADDR, {?IODIR, 16#f}, 2),
    %% reset pin value
    i2c:i2c_write(?I2C_CHANNEL, ?ADDR, {?GPIO, 0}, 2).

%% Runs the demo
run_demo() ->
    init(),
    start(),
    stop().

%% Main function
start() ->
    start(0).

start(Count) when Count < 16 ->
    %% turn on the led
    i2c:i2c_write(?I2C_CHANNEL, ?ADDR, {?GPIO, Count bsl 4}, 2),
    timer:sleep(1000),
    
    %% read DIP switches
    i2c:i2c_write(?I2C_CHANNEL, ?ADDR, {?GPIO}, 1),
    timer:sleep(10),
    {Val}=i2c:i2c_read(?I2C_CHANNEL, ?ADDR, 1),
    io:format("GPIO=~.16B : GP0: ~.16B  GP1: ~.16B  GP2 ~.16B  GP3 ~.16B~n",[Val, Val band 1, (Val band 2) bsr 1, (Val band 4) bsr 2, (Val band 8) bsr 3]),
    start(Count + 1);
start(_Count) ->
    ok.

stop() ->
    %% reset pin value
    i2c:i2c_write(?I2C_CHANNEL, ?ADDR, {?GPIO, 0}, 2),
    i2c:stop(?I2C_CHANNEL),
    i2c_sup:stop(),
    bye.
    
