-module(gpio_SUITE).

-compile(export_all).


all() ->
    [simple_output_test, simple_input_test,
     interrupt_raising, interrupt_falling, interrupt_both].

%%init_per_suite(Config) ->
    
init_per_suite(_Config) ->
    ok = application:start(gproc),
    [].

end_per_suite(_Config) ->
    ok = application:stop(gproc),
    ok.


init_per_testcase(_Case, Config) ->
    {ok, _} = gpio:start_link(),
    Config.

end_per_testcase(_Case, Config) ->
    ok = gpio:stop(),
    ok.


simple_output_test(_Config) ->
    ok = gpio:pin_init_output(1, false),
    Value = 1,
    ok = gpio:pin_write(1, Value),
    Value = sim_gpio:get_value(1),
    V2 = 0,
    ok = gpio:pin_write(1, V2),
    V2 = sim_gpio:get_value(1).

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
