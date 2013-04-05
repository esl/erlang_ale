-module(gpio_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).


all() ->
    [simple_output_test, simple_input_test%,
     %% interrupt_raising, interrupt_falling, interrupt_both
    ].

%%init_per_suite(Config) ->
    
init_per_suite(_Config) ->
    ok = application:start(gproc),
    [].

end_per_suite(_Config) ->
    ok = application:stop(gproc),
    ok.


init_per_testcase(simple_output_test, Config) ->
    C1 =  [{pin,1}, {value1,1}, {value2,0}|Config],
    mock_gpio(C1);
init_per_testcase(simple_input_test, Config) ->
    C1 = [{pin,2}, {value1, 1}, {value2,0}|Config],
    mock_gpio(C1).
    

end_per_testcase(_Case, Config) ->
    unmock_gpio(Config),
    ok.


mock_gpio(Config) ->
    meck:new(port_lib, [passthrough]),
    meck:expect(port_lib, load_driver, fun(_) -> ok end),
    Port =spawn( fun gpio_port_sim:gpio_port/0 ),
    meck:expect(port_lib, open_port,
                fun(_) ->
                        Owner = self(),
                        Port ! {context, Owner},
                        Port
                end),
    [{port_pid, Port}|Config].





unmock_gpio(_Config) ->
    meck:unload(port_lib),
    ok.

       
%%-------------------------------------------------------
%% Test cases
%%-------------------------------------------------------        

simple_output_test(Config) ->
    Pin = ?config(pin, Config),
    {ok, _Owner} = gpio:init(Pin, output),
    Value = ?config(value1, Config),
    ok = gpio:write(Pin, Value),
    true = meck:called(port_lib, call_to_port, ['_', '_', {write, Value}]),
    V2 = ?config(value2, Config),
    ok = gpio:write(Pin, V2),
    true = meck:called(port_lib, call_to_port, ['_', '_', {write, V2}]),
    ok = gpio:release(Pin).
    


    
simple_input_test(Config) ->
    Pin = ?config(pin, Config),
    {ok, _} = gpio:init(Pin, input),
    Value = ?config(value1, Config),
    Port = ?config(port_pid,Config),
    Port ! {set_value, Value},
    Value = gpio:read(Pin),
    V2 = ?config(value2, Config),
    Port ! {set_value, V2},
    V2 = gpio:read(Pin).

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
