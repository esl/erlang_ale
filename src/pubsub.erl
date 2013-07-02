-module(pubsub).

%% todo: must export sensible set the day we need these functions.
-export([set_interrupt/2,
         simulate_interrupt/2,
         pin_interrupts/1]).
          

%% @doc set_interrupt will create an interrupt for pin N on the
%% condition C.
set_interrupt(N, C) ->
    Key = {gpio_interrupt, N},
    case existing_interrupt_value(N) of
        V when V == no_interrupt;
               V == C ->
            gproc:reg({p, l, Key}, C),
            receive
                Msg ->
                    io:format("I got ~p.~n", [Msg])
            end;
        {error, _Reason} = Error ->
            Error;
        OtherC ->
            io:format("other interrupt active: ~p - ~p ~n", [N, OtherC]),
            {error, {other_interrupt_value_active, OtherC}}
    end.
        

%% @doc returns 'no_interrupt' or the interrupt value
existing_interrupt_value(N) ->
    PVs = gproc:lookup_local_properties({gpio_interrupt, N}),
    case unique_values_of_processes(PVs) of
        [] ->
            no_interrupt;
        [V] ->
            V;
        _ -> %% should not happen
            {error, multiple_interrupt_values_set}
    end.
            
unique_values_of_processes(PidValueList) ->
    lists:usort([ V || {_Pid, V} <- PidValueList ]).


simulate_interrupt(N, C) ->
    gproc:bcast({p, l, {gpio_interrupt, N}}, {N, C}).

%% @doc look up existing interrupts on a pin.
%%      Could be done simpler with gproc:lookup_local_properties/1. 
pin_interrupts(N) ->
    %% MatchHead = {p, l, '_'},
    Key = {gpio_interrupt, N},
    GProcKey = {p, l, Key},
    MatchHead = {GProcKey, '_', '_'},
    Guard = [],
    Result = ['$$'],
    gproc:select([{MatchHead, Guard, Result}]).




