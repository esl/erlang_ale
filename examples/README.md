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

For this example connect GPIO 27 to GPIO 22. GPIO 27 will be the interrupt pin,
and GPIO 22 will provides the signals to generate interrupts.
To generate interrupts just use ale_handler_gpio_int_test:press_btn/0 and ale_handler_gpio_int_test:release_btn/0
functions.
The default interrupt condition is rising, but it is possible change it by
ale_handler_gpio_int_test:set_int_condition/1 function, where the input parameter can be:
falling | rising | both | none | enabled | summarize

Step-by-step:

	cd example
	make all
	make shell

Start test application when rising interrupt edge will generates interrupt by ale_handler_gpio_int_test:start_link/0
	
	5> ale_handler_gpio_int_test:start_link().
	=INFO REPORT==== 22-Apr-2015::05:55:19 ===
	    "ALE driver process has been started and registered successfully."
	    drvModule: gpio
	    drvStartFunction: start
	    drvStartArgs: [27,input]
	    drvPid: <0.46.0>
	    monitorRef: #Ref<0.0.0.62>
	{ok,<0.44.0>}
	
Simulate button press event by 
	ale_handler_gpio_int_test:press_btn/0
	
	8> ale_handler_gpio_int_test:press_btn().
	=INFO REPORT==== 22-Apr-2015::05:55:51 ===
	    "ALE driver process has been started and registered successfully."
	    drvModule: gpio
	    drvStartFunction: start
	    drvStartArgs: [22,output]
	    drvPid: <0.50.0>
	    monitorRef: #Ref<0.0.0.83>
	ok
	9>
Rising interrupt event has been received by test application from driver:

	=INFO REPORT==== 22-Apr-2015::05:55:51 ===
	    "Interrupt has been occurred."
	    gpio: 27
	    interrupt_condition: rising

Release the button by ale_handler_gpio_int_test:release_btn/0. No interrupt event should be received by test application at this time.
	
	9> ale_handler_gpio_int_test:release_btn().
	ok
	10>
	
Press the button again and see, rising interrupt has been received again:

	10> ale_handler_gpio_int_test:press_btn().
	
	=INFO REPORT==== 22-Apr-2015::05:56:24 ===
	    "Interrupt has been occurred."
	    gpio: 27
	    interrupt_condition: rising
	ok

Change the interrupt condition to falling edge.

	11> ale_handler_gpio_int_test:set_int_condition(falling).
	ok

Release the pressed button. Falling interrupt event should be received by the test application:

	12> ale_handler_gpio_int_test:press_btn().
	ok
	12> ale_handler_gpio_int_test:release_btn().
	
	=INFO REPORT==== 22-Apr-2015::05:56:42 ===
	    "Interrupt has been occurred."
	    gpio: 27
	    interrupt_condition: falling
	ok
	13>
