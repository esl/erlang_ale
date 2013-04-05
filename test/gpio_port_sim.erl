%% @doc This is a very simplistic simulation of how a GPIO port
%% behaves to be used when testing the functionality of the Erlang
%% code in gpio.
-module(gpio_port_sim).



-compile(export_all).

-record(state,
        {owner,
         value,
         interrupt=none}).

%% @doc simple simulation of a GPIO pin port for testing purposes.
gpio_port() ->
    receive
        {context, Owner} ->
            gpio_port(#state{owner=Owner})
    end.

port_reply(Msg) ->
    {self(), {data, Msg}}.

gpio_port(#state{owner=Owner, value=undefined}=S) ->
    receive
        {Owner, {command, {init, _Pin, _Dir}}} ->
            Owner ! port_reply(ok),
            gpio_port(S#state{value=0})
    end;
gpio_port(#state{owner=Owner, value=Value, interrupt=Cond}=S) ->
    receive
        {Owner, {command, Cmd}} ->
                case Cmd of 
                    {call, From, {write, V}} ->
                        Owner ! port_reply({port_reply, From, ok}),
                        gpio_port(S#state{value=V});
                    {call, From, read} ->
                        Owner ! port_reply({port_reply, From, Value}),
                        gpio_port(S);
                    {call, From, {set_int, Condition}} ->
                        Owner ! port_reply({port_reply, From, ok}),
                        gpio_port(S#state{interrupt=Condition});
                    release ->
                        Owner ! port_reply(ok),
                        ok
                    end;
        %% pure testing
        {set_value, V} ->
            case value_change(Value, V) of
                Cond ->
                    Owner ! port_reply({gpio_interrupt, Cond});
                C when C /= no_change,
                       Cond == both ->
                    Owner ! port_reply({gpio_interrupt, Cond});
                _ ->
                    ok
            end,
            gpio_port(S#state{value=V})
    end.

value_change(0, 1) ->
    rising;
value_change(1, 0) ->
    falling;
value_change(_, _) ->
    no_change.
