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

# FAQ

1. Where did PWM support go?

The original Erlang/ALE implementation supported PWM on the Raspberry Pi. The
implementation was platform-specific and not maintained. After a year of bit
rot, it was removed.
