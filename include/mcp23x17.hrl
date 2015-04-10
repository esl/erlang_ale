%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% 
%% This header file contains all definitions of MCP23x17 port expander
%% chip. This chip has two kind of variant on communication interface
%% point of view.
%% The MCP23017 has I2C interface, and the MCP23S17 works with SPI.
%% This header file contains definitions for both, and the related erl 
%% file contains the driver functions for both chip.
%% 
%% All these are ported from my old Pic MCU project, where I have been used
%% these.
%% @end

-ifndef(MCP23x17_HRL).
-define(MCP23x17_HRL,true).

%% ====================================================================
%% Include
%% ====================================================================
-include("ale_type_def.hrl").

-type hw_addr()			::	integer().	%% The default HW address of MCP23x17 device is 16#20.
										%% This means all external address pins (A2..A0) are connected GROUND (logical level 0).
										%% Example for addressing:
										%% 		B7 B6 B5 B4 B3 B2 B1 B0
										%% 		0  1  x  x  A2 A1 A0 R/W
										%% Good to know:
										%%		This driver module will take care to compute value of R/W bit, thus HW address MUST be set
										%%		as A0 bit is the B0, A1 is the B1, and so on.
										%%		Example:
										%%			If B7=0, B6=1, B5=B4=0, A2=1, A1=0, A0=1 => 2#0100101R/W the address value what must user
										%%			as HwAddr is 2#00100101 == 16#25.
-type reg_addr()		::	integer().
-type reg_value()		::	integer(). 

-type timer_in_msec()	::	integer().	%% Timer value in msec.

%% ====================================================================
%% Possible serial communication interfaces
%% MCP23017 - I2C
%% MCP23S17 - SPI
%% ====================================================================
-type mcp23x17_comm_type_spi0()	::	spi0.
-type mcp23x17_comm_type_i2c1()	::	i2c1.
-type select_spi_dev_mfa()		::	mfa().
-type unselect_spi_dev_mfa()	::	mfa().
-type mcp23x17_comm_type()		::	mcp23x17_comm_type_spi0() | {mcp23x17_comm_type_spi0(), select_spi_dev_mfa(), unselect_spi_dev_mfa()} | mcp23x17_comm_type_i2c1().
-define(MCP23X17_COMM_TYPE_SPI0,  spi0).
-define(MCP23X17_COMM_TYPE_I2C1,  i2c1).

%% ====================================================================
%% Possible port sides
%% ====================================================================
-type mcp23x17_port_a()	::	'A'.
-type mcp23x17_port_b()	::	'B'.
-type mcp23x17_port()	::	mcp23x17_port_a() | mcp23x17_port_b().
-define(MCP23X17_PORT_A, 'A').
-define(MCP23X17_PORT_B, 'B').

%% ====================================================================
%% Pins of port
%% ====================================================================
-type mcp23x17_pin()	::	0..7.
-define(MCP23X17_NUMBER_OF_PORT_PER_SIDE,  8).
-define(MCP23X17_PIN0, 0).
-define(MCP23X17_PIN1, 1).
-define(MCP23X17_PIN2, 2).
-define(MCP23X17_PIN3, 3).
-define(MCP23X17_PIN4, 4).
-define(MCP23X17_PIN5, 5).
-define(MCP23X17_PIN6, 6).
-define(MCP23X17_PIN7, 7).

%% ====================================================================
%% Interrupt polarity
%% ====================================================================
-type mcp23x17_int_pol_high()	::	1.
-type mcp23x17_int_pol_low()	::	0.
-type mcp23x17_int_pol()		::	mcp23x17_int_pol_high() | mcp23x17_int_pol_low().
-define(MCP23X17_INT_POL_ACTIVE_HIGH,  1).
-define(MCP23X17_INT_POL_ACTIVE_LOW,   0).

%% ====================================================================
%% Interrupt mirroring
%% ====================================================================
-type mcp23x17_int_pin_mirrored()		::	1.
-type mcp23x17_int_pin_notmirrored()	::	0.
-type mcp23x17_int_pin_mirror()			::	mcp23x17_int_pin_mirrored() | mcp23x17_int_pin_notmirrored().
-define(MCP23X17_INT_PINS_MIRRORED,		1).
-define(MCP23X17_INT_PINS_NOT_MIRRORED,	0).

%% ====================================================================
%% I/O direction
%% ====================================================================
-type mcp23x17_io_input()	::	1.
-type mcp23x17_io_output()	::	0.
-type mcp23x17_io_dir()		::	mcp23x17_io_input() | mcp23x17_io_output().
-define(MCP23X17_IO_INPUT,	1).
-define(MCP23X17_IO_OUTPUT,	0).

%% ====================================================================
%% INPUT POLARITY
%% This register allows the user to configure the polarity on
%% the corresponding GPIO port bits.
%% If a bit is set, the corresponding GPIO register bit will
%% reflect the inverted value on the pin.
%% 1 = GPIO register bit will reflect the opposite logic state of the input pin.
%% 0 = GPIO register bit will reflect the same logic state of the input pin.
%% ====================================================================
-type mcp23x17_in_pol_opposite_of_pin_logic()	::	1.
-type mcp23x17_in_pol_same_of_pin_logic()		::	0.
-type mcp23x17_in_pol()							::	mcp23x17_in_pol_opposite_of_pin_logic() | mcp23x17_in_pol_same_of_pin_logic().
-define(MCP23X17_INPUT_POL_OPPOSITE_OF_PIN_LOGIC,    1).
-define(MCP23X17_INPUT_POL_SAME_OF_PIN_LOGIC,        0).

%% ====================================================================
%% INTERRUPT-ON-CHANGE
%% ====================================================================
-type mcp23x17_int_on_changed_en()	::	1.
-type mcp23x17_int_on_changed_dis()	::	0.
-type mcp23x17_int_on_changed()		::	mcp23x17_int_on_changed_en() | mcp23x17_int_on_changed_dis().
-define(MCP23X17_INTERRUPT_ON_CHANGE_ENABLED,  1).
-define(MCP23X17_INTERRUPT_ON_CHANGE_DISABLED, 0).

%% ====================================================================
%% DEFAULT COMPARE REGISTER FOR INTERRUPT-ON-CHANGE
%% The default comparison value is configured in the
%% DEFVAL register. If enabled (via GPINTEN and
%% INTCON) to compare against the DEFVAL register, an
%% opposite value on the associated pin will cause an
%% interrupt to occur.
%% ====================================================================
-type mcp23x17_def_comp_val_high()	::	1.
-type mcp23x17_def_comp_val_low()	::	0.
-type mcp23x17_def_comp_val()		::	mcp23x17_def_comp_val_high() | mcp23x17_def_comp_val_low().
-define(MCP23X17_DEF_COMP_VALUE_HIGH,	1).
-define(MCP23X17_DEF_COMP_VALUE_LOW,	0).

%% ====================================================================
%% INTERRUPT CONTROL REGISTER
%% The INTCON register controls how the associated pin
%% value is compared for the interrupt-on-change feature.
%% If a bit is set, the corresponding I/O pin is compared
%% against the associated bit in the DEFVAL register. If a
%% bit value is clear, the corresponding I/O pin is compared
%% against the previous value.
%% ====================================================================
-type mcp23x17_ioc_comp_with_def_comp_val()	::	1.
-type mcp23x17_ioc_comp_with_prev_pin_val()	::	0.
-type mcp23x17_ioc_comp_val()				::	mcp23x17_ioc_comp_with_def_comp_val() | mcp23x17_ioc_comp_with_prev_pin_val().
-define(MCP23X17_IOC_COMP_WITH_DEF_COMP_VALUE,   1).
-define(MCP23X17_IOC_COMP_WITH_PREV_PIN_VALUE,   0).

%% ====================================================================
%% PULL-UP RESISTOR
%% The GPPU register controls the pull-up resistors for the
%% port pins. If a bit is set and the corresponding pin is
%% configured as an input, the corresponding port pin is
%% internally pulled up with a 100 k? resistor.
%% ====================================================================
-type mcp23x17_pull_up_res_en()	::	1.
-type mcp23x17_pull_up_res_dis()::	0.
-type mcp23x17_pull_up_res()	::	mcp23x17_pull_up_res_en() | mcp23x17_pull_up_res_dis().
-define(MCP23X17_PULL_UP_RES_ENABLED,  1).
-define(MCP23X17_PULL_UP_RES_DISABLED, 0).

%% ====================================================================
%% Logical level of Pin. It can be LOW | HIGH
%% ====================================================================
-type mcp23x17_io_logical_low()		::	0.
-type mcp23x17_io_logical_high()	::	1.
-type mcp23x17_io_logical_level()	::	mcp23x17_io_logical_low() | mcp23x17_io_logical_high().
-define(MCP23X17_IO_LOGICAL_LOW, 0).
-define(MCP23X17_IO_LOGICAL_HIGH, 1).

%% ====================================================================
%% Header file for MCP23017/MCP23S17 I2C IO Expander
%% WARNING: BANK=0
%% ====================================================================
-define(IODIRA_ADDR,   16#00).
-define(IODIRB_ADDR,   16#01).
-define(IPOLA_ADDR,    16#02).
-define(IPOLB_ADDR,    16#03).
-define(GPINTENA_ADDR, 16#04).
-define(GPINTENB_ADDR, 16#05).
-define(DEFVALA_ADDR,  16#06).
-define(DEFVALB_ADDR,  16#07).
-define(INTCONA_ADDR,  16#08).
-define(INTCONB_ADDR,  16#09).
-define(IOCON_ADDR,    16#0A).
%%-define(IOCON_ADDR,    16#0B).
-define(GPUA_ADDR,     16#0C).
-define(GPUB_ADDR,     16#0D).
-define(INTFA_ADDR,    16#0E).
-define(INTFB_ADDR,    16#0F).
-define(INTCAPA_ADDR,  16#10).
-define(INTCAPB_ADDR,  16#11).
-define(GPIOA_ADDR,    16#12).
-define(GPIOB_ADDR,    16#13).
-define(OLATA_ADDR,    16#14).
-define(OLATB_ADDR,    16#15).

%% Bits of IOCON register
-define(IOCON_DUMMY,	0).
-define(IOCON_INTPOL,	1).
-define(IOCON_ODR,		2).
-define(IOCON_HAEN,		3).
-define(IOCON_DISSLW,	4).
-define(IOCON_SEQOP,	5).
-define(IOCON_MIRROR,	6).
-define(IOCON_BANK,		7).

-define(MCP23X17_PIN_TABLE, mcp23x17_pin_table).
-define(MCP23X17_PIN_TABLE_KEYPOS, 2).
-record(rMCP23x17_PORT_PIN, {
							 pinId,		%% tuple, {CommType,HWAddr,Port,PinNumber}
							 direction	%% mcp23x17_io_dir()							 
							 }).

-endif.