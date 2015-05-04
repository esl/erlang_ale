# Erlang/ALE -- Erlang Actor Library for Embedded

[![Build Status](https://travis-ci.org/esl/erlang_ale.svg)](https://travis-ci.org/esl/erlang_ale)

Erlang/ALE provides high level abstractions for interfacing to hardware
peripherals through I2C, SPI, and GPIOs on embedded platforms.

# Getting started

Erlang/ALE supports both rebar3 and erlang.mk build tools. If you're natively compiling Erlang/ALE, everything should work like any other
Erlang library. Normally, you would include `erlang_ale` as a dependency in your
`rebar.config` or Makefile file. If you just want to try it out, do the following:

    git clone https://github.com/esl/erlang_ale.git
    
    Steps for rebar3 build tool:
    	rebar3 compile
    	rebar3 shell
    
    Steps for erlang.mk build tool:
    	make
    	make shell

If you're cross-compiling, you'll need to setup your environment so that the
right C compiler is called. See the `Makefile` for the variables that will need
to be overridden.

# Documentation

It is possible generate HTML documentation from the self documented source.
	
	cd erlang_ale
	make docs

The HTML files can be seen in the erlang_ale/doc folder. Open the index.html file, and read.

# Examples

The following examples were tested on a
Raspberry Pi that was connected to an [Erlang Embedded Demo
Board](http://solderpad.com/omerk/erlhwdemo/). There's nothing special about
either the demo board or the Raspberry Pi, so these should work similarly on
other embedded Linux platforms.

## GPIO

A GPIO is just a wire that you can use as an input or an output. It can only be
one of two values, 0 or 1. A 1 corresponds to a logic high voltage like 3.3 V
and a 0 corresponds to 0 V. The actual voltage depends on the hardware.

Here's an example setup:

![GPIO schematic](doc/images/schematic-gpio.png)

To turn on the LED that's connected to the net labelled
`PI_GPIO18`, you can run the following:

    1> {ok, Gpio18} = gpio:start_link(18, output).
    {ok, <0.96.0>}

    2> gpio:write(Gpio18, 1).
    ok

Input works similarly:

    3> {ok, Gpio17} = gpio:start_link(17, input).
    {ok, <0.97.0>}

    4> gpio:read(Gpio17).
    0

    %% Push the button down

    5> gpio:read(Gpio17).
    1

If you'd like to get a message when the button is pressed or released, register
a process using the `register_int/1` or `register_int/2`. Then enable interrupts:

    6> gpio:register_int(Gpio17).
    ok

    7> gpio:set_int(Gpio17, both).
    ok

    %% Press the button a few times

    8> flush().
    {gpio_interrupt, 17, rising}
    {gpio_interrupt, 17, falling}
    {gpio_interrupt, 17, rising}
    {gpio_interrupt, 17, falling}
    ok

## SPI

A SPI bus is a common multi-wire bus used to connect components on a circuit
board. A clock line drives the timing of sending bits between components. Bits
on the `MOSI` line go from the master (usually the processor running Linux) to
the slave, and bits on the `MISO` line go the other direction. Bits transfer
both directions simultaneously. However, much of the time, the protocol used
across the SPI bus has a request followed by a response and in these cases, bits
going the "wrong" direction are ignored.

The following shows an example ADC that reads from either a temperature sensor
on CH0 or a potentiometer on CH1.

![SPI schematic](doc/images/schematic-adc.png)

The protocol for talking to the ADC is described in the [MCP3203
Datasheet](http://www.microchip.com/wwwproducts/Devices.aspx?dDocName=en010532).
Sending a 0x64 first reads the temperature and sending a 0x74 reads the
potentiometer.

NOTE: Before you try using SPI, make sure that the proper drivers are loaded.
For example, on Raspian be sure to run `raspi-config` to enable SPI.

    1> {ok, MySpi} = spi:start_link("spidev0.0", []).
    {ok, <0.124.0>}

    %% Read the potentiometer

    %% Use binary pattern matching to pull out the ADC counts (low 12 bits)
    2> <<_:4, Counts:12>> = spi:transfer(MySpi, <<16#74, 16#00>>).
    <<1, 197>>

    3> Counts.
    453

    %% Convert counts to volts (1023 = 3.3 V)
    4> Volts = Counts / 1023 * 3.3.
    1.461290322580645

## I2C

An I2C bus is similar to a SPI bus in function, but uses one less wire. It
supports addressing hardware components and bidirectional use of the data line.
Be aware that I2C requires some care when hooking up such as making sure to
attach the proper pullup resisters to SCL and SDA. If you using a Raspberry
Pi, see the [clock-stretching bug](http://www.advamation.com/knowhow/raspberrypi/rpi-i2c-bug.html).

The following shows a bus IO expander connected via I2C to the processor.

![I2C schematic](doc/images/schematic-i2c.png)

The protocol for talking to the IO expander is described in the [MCP23008
Datasheet](http://www.microchip.com/wwwproducts/Devices.aspx?product=MCP23008).
Here's a simple example of using it.

NOTE: Before trying the example below, make sure that I2C support has been
enabled and that the `i2c-dev` module has been inserted.

    %% On the Raspberry Pi, the IO expander is connected to I2C bus 1 (i2c-1).
    %% Its 7-bit address is 0x20. (see datasheet)
    1> {ok, IoExpander} = i2c:start_link("i2c-1", 16#20).
    {ok, <0.102.0>}

    %% By default, all 8 GPIOs are set to inputs. Set the 4 high bits to outputs
    %% so that we can toggle the LEDs. (Write 0x0f to register 0x00)
    2> i2c:write(IoExpander, <<16#00, 16#0f>>).
    ok

    %% Turn on the LED attached to bit 4 on the expander. (Write 0x10 to register
    %% 0x09)
    3> i2c:write(IoExpander, <<16#09, 16#10>>).
    ok

    %% Read all 11 of the expander's registers to see that the bit 0 switch is
    %% the only one on and that the bit 4 LED is on.
    4> i2c:write(IoExpander, <<16#00>>).  % Set the next register to be read to 0
    ok

    5> i2c:read(IoExpander, 11).
    <<15, 0, 0, 0, 0, 0, 0, 0, 0, 17, 16>>

    %% The 17 in register 9 says that bits 0 and bit 4 are high
    %% We could have just read register 9.

    6> i2c:write(IoExpander, <<9>>).
    ok

    7> i2c:read(IoExpander, 1).
    <<17>>

    %% A simpler way to write the register and read its contents.
    8> i2c:write_read(IoExpander, <<9>>, 1).
    <<17>>

# General supervision of started GPIO, I2C and SPI divers, processes

With this feature applications running on top of Erlang/ALE, can be sure the started GPIO, I2C and SPI
drivers will automatically restart when those are terminating for any reason, expect the controlled shutdown
initiated by the application on top of Erlang/ALE.
	
	Below the possible features are described:
		- Read GPIO logical value. 
			ale_handler:gpio_read(pin()).
			
			4> ale_handler:gpio_read(17).
			
			=INFO REPORT==== 4-May-2015::06:59:03 ===
			    "ALE driver process has been started and registered successfully."
			    initialMFA: {ale_handler,gpio_read,[17]}
			    drvModule: gpio
			    drvStartFunction: start_link
			    drvStartArgs: [17,input]
			    drvPid: <0.45.0>
			    module: ale_handler
			    line: 730
			0

		- Write logical value to a GPIO:
			Note: A GPIO configured for READING its logical value, cannot be used. Must release that GPIO first.
			
			5> ale_handler:gpio_write(17,1).
			{error,{gpio_is_aready_used_for_read,{pinDirection,input}}}
			6>
			6> ale_handler:gpio_release(17).
			
			=INFO REPORT==== 4-May-2015::06:59:14 ===
			    "ALE driver has been released."
			    drvPid: <0.45.0>
			    initialMFA: {ale_handler,gpio_read,[17]}
			    drvInitMFA: {gpio,start_link,[17,input]}
			    module: ale_handler
			    line: 787
			ok
			7>
			7>
			7> ale_handler:gpio_write(17,1).
			
			=INFO REPORT==== 4-May-2015::06:59:17 ===
			    "ALE driver process has been started and registered successfully."
			    initialMFA: {ale_handler,gpio_write,[17,1]}
			    drvModule: gpio
			    drvStartFunction: start_link
			    drvStartArgs: [17,output]
			    drvPid: <0.50.0>
			    module: ale_handler
			    line: 730
			ok
			8>
			
		- Configure GPIO for interrupt:
			ale_handler:gpio_set_int(pin(), interrupt_condition()).
			ale_handler:gpio_set_int(pin(), interrupt_condition(), pid()).
			
			9> ale_handler:gpio_set_int(17,rising).
	
			=INFO REPORT==== 4-May-2015::07:03:01 ===
			    "ALE driver process has been started and registered successfully."
			    initialMFA: {ale_handler,gpio_set_int,[17,rising,<0.41.0>]}
			    drvModule: gpio
			    drvStartFunction: start_link
			    drvStartArgs: [17,input]
			    drvPid: <0.54.0>
			    module: ale_handler
			    line: 730
			ok
			10>
		
		- Release GPIO to configured for any above described purpose:
			ale_handler:gpio_release(pin).
			
			10> ale_handler:gpio_release(17).
	
			=INFO REPORT==== 4-May-2015::07:12:48 ===
			    "ALE driver has been released."
			    drvPid: <0.54.0>
			    initialMFA: {ale_handler,gpio_set_int,[17,rising,<0.41.0>]}
			    drvInitMFA: {gpio,start_link,[17,input]}
			    module: ale_handler
			    line: 787
			ok
			11>

		- I2C write
			Write data into the specified I2C device. This will open the I2C driver and assign it to a process automatically.
			The supervisor will restart the driver if that terminates for some reason.
			
			ale_handler:i2c_write(devname(), addr(), data()).
			
			Write binary 25 into register 0 of MCP23017 IO expander device.
			
			1> ale_handler:i2c_write("i2c-1", 32, <<0,25>>).

			=INFO REPORT==== 4-May-2015::07:39:10 ===
			    "ALE driver process has been started and registered successfully."
			    initialMFA: {ale_handler,i2c_write,["i2c-1",32,<<0,25>>]}
			    drvModule: i2c
			    drvStartFunction: start_link
			    drvStartArgs: ["i2c-1",32]
			    drvPid: <0.37.0>
			    module: ale_handler
			    line: 732
			ok
			
		- I2C read
			Read data from the specified I2C device. This will open the I2C driver and assign it to a process automatically.
			The supervisor will restart the driver if that terminates for some reason.
			
			ale_handler:i2c_read(devname(), addr(), len()).
			
			Example to read register 0 of MCP23017 IO expander device.
			
			2>
			2> ale_handler:i2c_write("i2c-1", 32, <<0>>).
			ok
			3> ale_handler:i2c_read("i2c-1", 32, 1).
			{ok,<<25>>}
			4>
			
		- I2C stop
			Stop I2C driver process.
			
			4> ale_handler:i2c_stop("i2c-1", 32).
			
			=INFO REPORT==== 4-May-2015::07:43:23 ===
			    "ALE driver has been released."
			    drvPid: <0.37.0>
			    initialMFA: {ale_handler,i2c_write,["i2c-1",32,<<0,25>>]}
			    drvInitMFA: {i2c,start_link,["i2c-1",32]}
			    module: ale_handler
			    line: 789
			ok
			5>
			
		- Demonstrate how I2C driver will restart automatically when driver process terminates
		
			1> ale_handler:i2c_write("i2c-1", 32, <<0>>).
	
			=INFO REPORT==== 4-May-2015::07:58:31 ===
			    "ALE driver process has been started and registered successfully."
			    initialMFA: {ale_handler,i2c_write,["i2c-1",32,<<0>>]}
			    drvModule: i2c
			    drvStartFunction: start_link
			    drvStartArgs: ["i2c-1",32]
			    drvPid: <0.37.0>
			    module: ale_handler
			    line: 732
			ok
			2> ale_handler:i2c_read("i2c-1", 32, 1).
			{ok,<<25>>}
			3>
			3> erlang:exit(erlang:list_to_pid("<0.37.0>"),kill).
			true
			4>
			=INFO REPORT==== 4-May-2015::07:59:10 ===
			    "ALE driver process has been started and registered successfully."
			    initialMFA: {ale_handler,i2c_write,["i2c-1",32,<<0>>]}
			    drvModule: i2c
			    drvStartFunction: start_link
			    drvStartArgs: ["i2c-1",32]
			    drvPid: <0.41.0>
			    module: ale_handler
			    line: 732
			4>
			4> ale_handler:i2c_read("i2c-1", 32, 1).
			{ok,<<2>>}
			5> ale_handler:i2c_write("i2c-1", 32, <<0>>).
			ok
			6> ale_handler:i2c_read("i2c-1", 32, 1).
			{ok,<<25>>}

		- SPI transfer
			- Write 25 into register 0 of MCP23S17 SPI IO expander device
			
				4> ale_handler:spi_transfer("spidev0.0", [], <<64, 0, 25>>).
				
				=INFO REPORT==== 4-May-2015::08:35:07 ===
				    "ALE driver process has been started and registered successfully."
				    initialMFA: {ale_handler,spi_transfer,["spidev0.0",[],<<64,0,25>>]}
				    drvModule: spi
				    drvStartFunction: start_link
				    drvStartArgs: ["spidev0.0",[]]
				    drvPid: <0.42.0>
				    module: ale_handler
				    line: 732
				{ok,<<0,0,25>>}
				5>
				
			- Read register 0 of MCP23S17 SPI IO expander device
				6> ale_handler:spi_transfer("spidev0.0", [], <<65, 0, 0>>).
				{ok,<<0,0,25>>}
				7>

		- Demonstrate how SPI driver will restart automatically when driver process terminates
		
			8> erlang:exit(erlang:list_to_pid("<0.42.0>"),kill).
			true
			9>
			=INFO REPORT==== 4-May-2015::08:43:06 ===
			    "ALE driver process has been started and registered successfully."
			    initialMFA: {ale_handler,spi_transfer,["spidev0.0",[],<<64,0,25>>]}
			    drvModule: spi
			    drvStartFunction: start_link
			    drvStartArgs: ["spidev0.0",[]]
			    drvPid: <0.48.0>
			    module: ale_handler
			    line: 732
			
			9> ale_handler:spi_transfer("spidev0.0", [], <<65, 0, 0>>).
			{ok,<<0,0,25>>}
			10>

		- SPI stop. Stop SPI driver.
		
			10> ale_handler:spi_stop("spidev0.0").
			
			=INFO REPORT==== 4-May-2015::08:43:59 ===
			    "ALE driver has been released."
			    drvPid: <0.48.0>
			    initialMFA: {ale_handler,spi_transfer,["spidev0.0",[],<<64,0,25>>]}
			    drvInitMFA: {spi,start_link,["spidev0.0",[]]}
			    module: ale_handler
			    line: 789
			ok
			11>

# Support for MCP23017 and MCP23S17 IO expander device

The next features of MCP23x17 devices are implemented:
	
	- Configure and enable/disable interrupt on a PIN
		mcp23x17:setup_interrupt/8, mcp23x17:enable_interrupt/4, mcp23x17:disable_interrupt/4
		
	- Configure IO PIN direction
		mcp23x17:setup_io_direction/5
		
	- Set logical level of an output configured IO PIN
		mcp23x17:setup_io_logical_level/5
		
	- Read logical level of an input configured IO PIN
		mcp23x17:get_io_logical_level/4
	
More can be seen in the example/ex_mcp23x17.erl module.

# Support for MCP7940n RTC device

The following features are implemented:
	
	- Start general gen_server for MCP794n RTC and do the basic setup:
		Start the server 24H mode and enable VBat function. These are necessary for future configuration, mainly when configure Date and Time and alarms. The current local Date and Time will be configured automatically if the next conditions are met, otherwise Date and Time will not set automatically, but it is always possible to do by calling the function mcp7940n:date_and_time_set/1.
		
		- VBat disabled or
		- VBat enabled but main power of RTC device was lost for same reason. The PWRFAIL bit equals with 1.
		
		1> mcp7940n:start(0,1).
		
		=INFO REPORT==== 4-May-2015::12:28:53 ===
		    "ALE driver process has been started and registered successfully."
		    initialMFA: {ale_handler,i2c_write,["i2c-1",111,<<3>>]}
		    drvModule: i2c
		    drvStartFunction: start_link
		    drvStartArgs: ["i2c-1",111]
		    drvPid: <0.38.0>
		    module: ale_handler
		    line: 732
		
		=INFO REPORT==== 4-May-2015::12:28:53 ===
		    "Alarm interrupt has been disabled"
		    alarmId: 0
		    module: mcp7940n
		    line: 1307
		
		=INFO REPORT==== 4-May-2015::12:28:53 ===
		    "Alarm interrupt has been disabled"
		    alarmId: 1
		    module: mcp7940n
		    line: 1307
		
		=INFO REPORT==== 4-May-2015::12:28:53 ===
		    "RTC oscillator has been started"
		    module: mcp7940n
		    line: 1627
		
		=INFO REPORT==== 4-May-2015::12:28:54 ===
		    "RTC has been started"
		    module: mcp7940n
		    line: 551
		{ok,<0.35.0>}

	- Configure CONTROL register bits
		OUT, SQWEN, ALM1EN, ALM0EN, EXTOSC, CRSTRIM, SQWFS1, SQWFS0
		
	- Configure Date&Time. Both 12H and 24H modes are supported.
		
		2> LocalTime=calendar:local_time().
		{{2015,5,4},{12,29,25}}
		3> mcp7940n:date_and_time_set(LocalTime).
		
		=INFO REPORT==== 4-May-2015::12:29:40 ===
		    "RTC oscillator has been stopped"
		    module: mcp7940n
		    line: 1650
		
		=INFO REPORT==== 4-May-2015::12:29:40 ===
		    "Alarm interrupt has been disabled"
		    alarmId: 0
		    module: mcp7940n
		    line: 1307
		
		=INFO REPORT==== 4-May-2015::12:29:40 ===
		    "Alarm interrupt has been disabled"
		    alarmId: 1
		    module: mcp7940n
		    line: 1307
		
		=INFO REPORT==== 4-May-2015::12:29:40 ===
		    "Alarm interrupt has been disabled"
		    alarmId: 0
		    module: mcp7940n
		    line: 1307
		
		=INFO REPORT==== 4-May-2015::12:29:40 ===
		    "Alarm interrupt has been disabled"
		    alarmId: 1
		    module: mcp7940n
		    line: 1307
		
		=INFO REPORT==== 4-May-2015::12:29:40 ===
		    "RTC oscillator has been started"
		    module: mcp7940n
		    line: 1627
		
		=INFO REPORT==== 4-May-2015::12:29:40 ===
		    "Date and Time has been configured in RTC"
		    dateAndTime: {{2015,5,4},{12,29,25}}
		    module: mcp7940n
		    line: 1720
		ok
		
	- Read current Date and Time in RTC device
	
		16> mcp7940n:date_and_time_get().
		{ok,{{2015,5,4},{12,53,31}}}
		17>
	
	- Configure Alarm modules
		Alarm-0, Alarm-1
		
		Below example shows how to configure Alarm-0 module. The Alarm time will be local data and time + 1 minute. Alarm will be generated when sec, min, hour, wday attributes are matches. The alarm logical polarity will be HIGH (=1).
		
		7> LocalDateTime=calendar:local_time().
		{{2015,5,4},{12,31,11}}
		8> {Date,{H,M,S}} = LocalDateTime.
		{{2015,5,4},{12,31,11}}
		9> Alarm0DateTime={Date,{H,M+1,S}}.
		{{2015,5,4},{12,32,11}}
		10> mcp7940n:alarm_configure(0, Alarm0DateTime, 2#111, 1, 1).
		
		=INFO REPORT==== 4-May-2015::12:31:19 ===
		    "Alarm interrupt has been enabled"
		    alarmId: 0
		    module: mcp7940n
		    line: 1275
		
		=INFO REPORT==== 4-May-2015::12:31:19 ===
		    "Alarm interrupt has been configured"
		    alarmId: 0
		    module: mcp7940n
		    line: 1444
		ok
		
	- Check alarm status by command if MFP PIN of RTC device does not wired to anywhere
	
		13> mcp7940n:alarm_interrupt_flag_check(0).
		{ok,0} -----> There is no Alarm on Alarm-0 module.
		
		14> mcp7940n:alarm_interrupt_flag_check(0).
		{ok,1} -----> Alarm has been triggered on Alarm-0 module.
	
	- Supervise main power of RTC device
		It is possible subscribe/unsubscribe for "Main Power change notification". The following notifications may received in the application who has subscribed on this event:
		
		{main_power_is_lost} - when Main power of RTC device is lost.
		{main_power_is_back, DateTimeWhenPowerLost, DateTimeWhenPowerBack} - when Main power of RTC device is back.
		
		17> mcp7940n:pwr_status_change_subscribe().
		
		=INFO REPORT==== 4-May-2015::13:00:23 ===
		    "Pid has been subscribed to the PWR change notification"
		    pid: <0.33.0>
		    module: mcp7940n
		    line: 658
		ok
		18>

	- Supervise oscillator module of RTC device
		It is possible subscribe/unsubscribe for "Change status of oscillator notification". The following notifications may received in the application who has subscribed on this event:
		
		{oscillator_is_running} - when RTC's oscillator is running properly.
		{oscillator_is_not_running, AnyReason} - when RTC's oscillator has some failure.
		
		18> mcp7940n:oscillator_status_change_subscribe().
		
		=INFO REPORT==== 4-May-2015::13:01:00 ===
		    "Pid has been subscribed to the OSCILLATOR change notification"
		    pid: <0.33.0>
		    module: mcp7940n
		    line: 685
		ok
		19>
		=INFO REPORT==== 4-May-2015::13:01:01 ===
		    "RTC oscillator is running"
		    module: mcp7940n
		    line: 862

	- Read/Write User Memory area
		
		19> mcp7940n:write_sram(32, 2).
		ok
		20> mcp7940n:read_sram(32).
		{ok,2}
		21>

More example can be seen in example/ex_mcp7940n.erl module. This documented in examples/README.md.

# FAQ

1. Where did PWM support go?

The original Erlang/ALE implementation supported PWM on the Raspberry Pi. The
implementation was platform-specific and not maintained. After a year of bit
rot, it was removed.
