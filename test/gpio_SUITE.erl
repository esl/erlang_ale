-module(gpio_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).


all() ->
    [simple_output_test, simple_input_test,
     interrupt_rising, interrupt_falling, interrupt_both
    ].


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
    mock_gpio(C1);
init_per_testcase(interrupt_rising, Config) ->
    C1 = [{pin,3}, {value1,0}, {value2,1}, {condition,rising}|Config],
    mock_gpio(C1);
init_per_testcase(interrupt_falling, Config) ->
    C1 = [{pin,4}, {value1,1}, {value2,0}, {condition,falling}|Config],
    mock_gpio(C1);
init_per_testcase(interrupt_both, Config) ->
    C1 = [{pin,5}, {value1,1}, {value2, 0}, {condition,both}|Config],
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

interrupt_rising(Config) ->
    one_interrupt(Config).

one_interrupt(Config) ->
    Pin = ?config(pin, Config),
    {ok, _} = gpio:init(Pin, input),
    Port = ?config(port_pid, Config),
    Condition = ?config(condition, Config),
    InitialValue = ?config(value1, Config),
    Port ! {set_value, InitialValue},
    ok = gpio:set_int(Pin, Condition),
    TriggerValue = ?config(value2, Config),
    Port ! {set_value, TriggerValue},
    ok = receive_interrupt(Pin, Condition).


interrupt_falling(Config) ->
    one_interrupt(Config).

interrupt_both(Config) ->
    Pin = ?config(pin, Config),
    {ok, _} = gpio:init(Pin, input),
    Port = ?config(port_pid, Config),
    Condition = ?config(condition, Config),
    ok = gpio:set_int(Pin, Condition),
    V1 = ?config(value1, Config),
    Port ! {set_value, V1},
    ok = receive_interrupt(Pin, Condition),
    V2 = ?config(value2, Config),
    Port ! {set_value, V2},
    ok = receive_interrupt(Pin, Condition).
    
  
receive_interrupt(Pin, Condition) ->
    receive
        {gpio_interrupt, Condition} ->
            ok;
        Unexpected ->
            {error, {unexpected, Unexpected}}
    after 500 ->
            ct:log("took too long to receive interrupt"),
            error
    end.
