# Overview

These examples demonstrate simple uses of Erlang/ALE. To compile then, just
run:

    make

The examples can be run via Makefile targets.

# gpio_counter

For this example, connect a button to GPIO 17 and another button to GPIO 22.
Connect GPIO 23 and GPIO 24 to LEDs. Pressing the button on GPIO 22 will increase
the "count" on the LEDs and the GPIO 17 button will decrease it.

# ale_handler_gpio_int_test

For this example connect GPIO 27 and GPIO 22. GPIO 27 will be the interrupt pin,
and GPIO 22 will provides the signals to generate interrupts. To generate interrupts
just use ale_handler_gpio_int_test:press_btn/0 and ale_handler_gpio_int_test:release_btn/0
functions. The default interrupt condition is rising, but it is possible change it by
ale_handler_gpio_int_test:set_int_condition/1 function, where the input parameter can be:
falling | rising | both | nonce | enabled | summarize

A similar log information will be printed in the Erlang shell once an interrupt has been occurred:
	=INFO REPORT==== 9-Apr-2015::06:31:20 ===
	    "Interrupt has been occurred."
	    gpio: 27
	    interrupt_condition: falling
