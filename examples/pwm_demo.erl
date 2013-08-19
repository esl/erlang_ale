%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc Changes the brightness of led from the min value to the max value and
%%% and come back.
%%% @end

-module(pwm_demo).

-define(MAX_PWM, 1024).
-define(MIN_PWM, 0).

-export([init/0, start/0, stop/0, fade_in/1, fade_out/1]).

%% Initialize the PWM peripheral
init() ->
    pwm_sup:start_link([{pwm1, "/dev/pwm1"}]).

%% Run the demo
start() ->
	io:format("Fade in..~n"),
	fade_in(?MIN_PWM),
	io:format("Fade out..~n"),
	fade_out(?MAX_PWM).

%% Release the PWM peripheral
stop() ->
    pwm:stop(pwm1),
    pwm_sup:stop(),
    bye.

%% Change the value of the led from the min value to the max value
fade_in(X) when X =< ?MAX_PWM ->
	 pwm:value(pwm1, X),
	 timer:sleep(2),
	 fade_in(X+1);
fade_in(_X) ->
	   ok.

%% Change the value of the led from the max value to the min value
fade_out(X) when X >= ?MIN_PWM ->
	    pwm:value(pwm1, X),
	    timer:sleep(2),
	    fade_out(X-1);
fade_out(_X) ->
	    ok.
	 
	 
