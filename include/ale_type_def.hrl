%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% @end

-ifndef(ALE_TYPE_DEF_HRL).
-define(ALE_TYPE_DEF_HRL,true).

-type pin()					:: non_neg_integer().
-type pin_direction()		:: 'input' | 'output'.
-type pin_state()			:: 0 | 1.
-type interrupt_condition()	:: 'enabled' | 'summarize' | 'none' | 'rising' | 'falling' | 'both'.
-type server_ref()			:: atom() | {atom(), atom()} | pid().

-type addr()				:: integer(). %% fix to be 2-127
-type data()				:: binary().
-type len()					:: integer().
-type devname()				:: string().

-define(PIN_DIRECTION_INPUT, input).
-define(PIN_DIRECTION_OUTPUT, output).

-define(PIN_STATE_HIGH, 1).
-define(PIN_STATE_LOW, 0).

-define(INT_COND_ENABLED, 'enabled').
-define(INT_COND_SUMMARIZE, 'summarize').
-define(INT_COND_NONE, 'none').
-define(INT_COND_RISING, 'rising').
-define(INT_COND_FALLING, 'falling').
-define(INT_COND_BOTH, 'both').

-endif.