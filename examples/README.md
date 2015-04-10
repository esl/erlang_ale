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
falling | rising | both | none | enabled | summarize

A similar log information will be printed in the Erlang shell once an interrupt has been occurred:

	=INFO REPORT==== 9-Apr-2015::06:31:20 ===
		"Interrupt has been occurred."
		gpio: 27
		interrupt_condition: falling

# ex_mcp23x17:start_i2c_blinking_led/4

This example module have few functions to introduce how to configure, control MCP23S17 and MCP23017 IO
expander devices.
	
	Blinking a Led connected to MCP23017 (I2C) IO expander:
		Before try out this example, the following preparation needs to be done:
			-  Connect MCP23017 IO expander Pins:
				PIN9  - VDD
				PIN10 - VSS
				PIN12 - I2C-SCL -> Raspberry Pi PIN5
				PIN13 - I2C-SDA -> Raspberry Pi PIN3
				PIN15 - VSS
				PIN16 - VSS
				PIN17 - VSS
				PIN18 - VDD
				PIN21 - LED with 470ohm serial resistor -> VSS
				
				Here is the schematic about circuit
![I2C LED BLINKING schematic](doc/images/schematic-ex_start_i2c_blinking_led.png)
		
		HwAddr	: 16#20, because all Ax PINs are connected to VSS (Ground)
		Port	: IO expander device has 2 Ports, 'A' and 'B'. In this example we are using Port 'A'.
		Pin		: There are 8 PINS on each port side. In this example we are using PIN0.
		Timer	: The periodic time of blinking in msec.
		
		Start blinking LED:
		-------------------
			1> ex_mcp23x17:start_i2c_blinking_led(16#20, 'A', 0, 200).
	
			=INFO REPORT==== 10-Apr-2015::06:05:57 ===
	    		"ALE driver process has been started and registered successfully."
	    		drvModule: i2c
	    		drvStartFunction: start
	    		drvStartArgs: ["i2c-1",32]
	    		drvPid: <0.39.0>
	    		monitorRef: #Ref<0.0.0.36>
			ok
		
		Stop blinking LED:
		------------------
			2> ex_mcp23x17:stop_i2c_blinking_led().
			ok
			3>
		
