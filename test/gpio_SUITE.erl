-module(gpio_SUITE).

-compile(export_all).


all() ->
    [simple_output_test
     %%    , simple_input_test,
     %% interrupt_raising, interrupt_falling, interrupt_both
    ].

%%init_per_suite(Config) ->
    
init_per_suite(_Config) ->
    ok = application:start(gproc),
    [].

end_per_suite(_Config) ->
    ok = application:stop(gproc),
    ok.


init_per_testcase(_Case, Config) ->
    %% {ok, _} = gpio:start_link(),
    Config.

end_per_testcase(_Case, _Config) ->
    %% ok = gpio:stop(),
    ok.


mock_gpio(_Config) ->
    meck:new(gpio, [passthrough]),
    meck:expect(gpio, load_driver, fun(_) -> ok end),
    meck:expect(gpio, send_to_port, fun(_,_) -> ok end),
    meck:expect(gpio, sync_call_to_port, fun(_,_) -> ok end),
    meck:expect(gpio, call_to_port, fun mocked_gpio_call_to_port/3),
    meck:expect(gpio, open_port, fun(_) -> some_port end),
    ok = gpio:load_driver(pap),
    ok.

unmock_gpio(_Config) ->
    meck:unload(gpio).
                                           
mocked_gpio_call_to_port(_State, From, {write,_}) ->
    ct:log("in mocked_gpio_call_to_port"),
    spawn( fun() ->
                   ct:log("reading to call from_port"),
                   gpio:from_port(1, {port_reply, From, ok})
           end),
    ok.
       

simple_output_test(Config) ->
    mock_gpio(Config),
    ok = gpio:load_driver("tehu"),
    {ok, _} = gpio:init(1, output),
    Value = 1,
    ok = gpio:write(1, Value),
    true = meck:called(gpio, call_to_port, ['_', '_', {write, Value}]),
    V2 = 0,
    ok = gpio:write(1, V2),
    true = meck:called(gpio, call_to_port, ['_', '_', {write, V2}]),
    ok = gpio:release(1),
    unmock_gpio(Config).
    
    
    
simple_input_test(_Config) ->
    ok = gpio:pin_init_input(2),
    Value = 1,
    ok = sim_gpio:set_value(2, Value),
    Value = gpio:pin_read(2),
    V2 = 0,
    ok = sim_gpio:set_value(2, V2),
    V2 = gpio:pin_read(2).

interrupt_raising(_Config) ->
    Pin = 3,
    ok = gpio:pin_init_input(Pin),
    Condition = raising,
    ok = gpio:pin_set_int(Pin, Condition),
    ok = sim_gpio:set_value(Pin, 1),
    ok = receive_interrupt(Pin, Condition).

interrupt_falling(_Config) ->
    Pin = 4,
    ok = gpio:pin_init_input(Pin),
    ok = sim_gpio:set_value(Pin, 1),
    Condition = falling,
    ok = gpio:pin_set_int(Pin, Condition),
    ok = sim_gpio:set_value(Pin, 0),
    ok = receive_interrupt( Pin, Condition).

interrupt_both(_Config) ->
    Pin = 5,
    ok = gpio:pin_init_input(Pin),
    Condition = both,
    ok = gpio:pin_set_int(Pin, Condition),
    ok = sim_gpio:set_value(Pin, 1),
    ok = receive_interrupt(Pin, Condition),
    ok = sim_gpio:set_value(Pin, 0),
    0 = gpio:pin_read(Pin),
    ok = receive_interrupt(Pin, Condition).
    
  
receive_interrupt(Pin, Condition) ->
    receive
        {gpio_interrupt, Pin, Condition} ->
            ok;
        Unexpected ->
            {error, {unexpected, Unexpected}}
    after 500 ->
            error
    end.
