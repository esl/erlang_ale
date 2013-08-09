-module(i2c_demo).

-export([init/0, run_demo/0, start/2, stop/1]).

-define(ADDR, 16#20).
-define(IODIR, 16#0).
-define(GPIO, 16#9).

init() ->
    i2c:start_link(),
    %% initialise i2c bus
    Fd = i2c:i2c_init(),
    %% set pin directions
    i2c:i2c_write(Fd, ?ADDR, {?IODIR, 16#f}, 2),
    %% reset pin value
    i2c:i2c_write(Fd, ?ADDR, {?GPIO, 0}, 2),
    Fd.
    
run_demo() ->
    Fd = init(),
    start(Fd, 0),
    stop(Fd).

start(Fd, Count) when Count < 16 ->
    %% set led state
    i2c:i2c_write(Fd, ?ADDR, {?GPIO, Count bsl 4}, 2),
    timer:sleep(1000),
    
    %% read DIP switches
    i2c:i2c_write(Fd, ?ADDR, {?GPIO}, 1),
    timer:sleep(10),
    {Val}=i2c:i2c_read(Fd, ?ADDR, 1),
    io:format("GPIO=~.16B : GP0: ~.16B  GP1: ~.16B  GP2 ~.16B  GP3 ~.16B~n",[Val, Val band 1, (Val band 2) bsr 1, (Val band 4) bsr 2, (Val band 8) bsr 3]),
    start(Fd, Count + 1);
start(_Fd, _Count) ->
    ok.

stop(Fd) ->
    %% reset pin value
    i2c:i2c_write(Fd, ?ADDR, {?GPIO, 0}, 2),
    i2c:stop(),
    bye.
    
