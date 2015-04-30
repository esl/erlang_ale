%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module provides interface to able to control MCP23S17 and MCP23017
%% IO expander devices.
%% @end


-module(mcp23x17).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% Includes
%% ====================================================================
-include("mcp23x17.hrl").
-include("ale_common.hrl").

%% ====================================================================
%% Defines
%% ====================================================================
-define(DUMMY_BYTE_FOR_READ, 0).
-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
		 %% Interrupt related functions
		 setup_interrupt/8, enable_interrupt/4, disable_interrupt/4, 
		 setup_int_polarity/3, setup_int_pin_mirror/3, read_interrupt/3,
		 setup_interrupt_on_change_ctrl/5, setup_default_compare_reg_for_int_on_change/5,
		 
		 %% Port IO related functions
		 setup_io_direction/5, setup_io_logical_level/5, get_io_logical_level/4,
		 
		 %% Pull up resistor
		 setup_pull_up_resistor/5,
		 
		 %% Input polarirty of IO pin 
		 setup_input_polarity/5
		]).

-export([i2c_driver_stop/2, spi_driver_stop/1]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% ====================================================================
%% Start/Stop Gen server 
%% ====================================================================

%% ====================================================================
%% @doc
%% Start MCP23x17 driver.
%% @end
-spec start_link() -> ok | {error, term()}.
%% ====================================================================
start_link() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Already started
			{ok, Pid};
		_->	%% The supervisor does not started yet.
			gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{timeout, 1000}]) 
	end.

%% ====================================================================
%% @doc Stop MCP23x17 driver.
%% @end
-spec stop() -> 'ok' | {'error', term()}.
%% ====================================================================
stop() ->
	case whereis(?SERVER) of
		Pid when is_pid(Pid) ->
			%% Process is already exists. Must stop it.
			gen_server:call(Pid, {stop}, infinity);
		
		_->	%% Process is not running.
			ok
	end. 

%% ====================================================================
%% Driver functions 
%% ====================================================================

%% ====================================================================
%% Setup and enable interrupt on Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%%	 IPol		: input polarity register/bit
%%				 	This register allows the user to configure the polarity on
%%				 	the corresponding GPIO Port bits.
%%				 	If a bit is set, the corresponding GPIO register bit will
%%				 	reflect the inverted value on the Pin.
%%				 	1 = GPIO register bit will reflect the opposite logic state of the input Pin.
%%				 	0 = GPIO register bit will reflect the same logic state of the input Pin.
%%	 PullUpRes	: Pull-Up Resistor
%%				   1: Pull-up enabled
%%				   0: Pull-up disabled
%%	 DefComp	: Default Compare value
%%				 	The default comparison value is configured in the
%%				 	DEFVAL register. If enabled (via GPINTEN and
%%				 	INTCON) to compare against the DEFVAL register, an
%%				 	opposite value on the associated Pin will cause an
%%				 	interrupt to occur.
%%				 	If the associated Pin level is the opposite from the register bit, an interrupt occurs.
%%	 IntCtrl	: interrupt control
%%					The INTCON register controls how the associated Pin
%%					value is compared for the interrupt-on-change feature.
%%				 	If a bit is set, the corresponding I/O Pin is compared
%%				 	against the associated bit in the DEFVAL register. If a
%%				 	bit value is clear, the corresponding I/O Pin is compared
%%				 	against the previous value.
%%				   	1 = Controls how the associated Pin value is compared for interrupt-on-change.
%%				   	0 = Pin value is compared against the previous Pin value.
-spec setup_interrupt(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin(), mcp23x17_in_pol(), mcp23x17_pull_up_res(), mcp23x17_def_comp_val(), mcp23x17_ioc_comp_val()) -> ok | {error, term()}.
%% ====================================================================
setup_interrupt(CommType, HwAddr, Port, Pin, IPol, PullUpRes, DefComp, IntCtrl) ->
	%% Setup direction of Port/Pin to input
	case setup_io_direction(CommType, HwAddr, Port, Pin, ?MCP23X17_IO_INPUT) of
		ok ->
			%% Setup input polarity
			case setup_input_polarity(CommType, HwAddr, Port, Pin, IPol) of
				ok ->
					%% Setup Pull-Up resistor
					case setup_pull_up_resistor(CommType, HwAddr, Port, Pin, PullUpRes) of
						ok ->
							%% Setup default compare register for interrupt-on-change.
							case setup_default_compare_reg_for_int_on_change(CommType, HwAddr, Port, Pin, DefComp) of
								ok -> 
									%% Setup INTCON (Interrupt-On-Change Control) register.
									%% By default interrupt will occurred when IO Pin logic level
									%% matches with value of corresponding bit in DEFVAL register.
									case setup_interrupt_on_change_ctrl(CommType, HwAddr, Port, Pin, IntCtrl) of
										ok ->
											%% Enable interrupt on PIN
											enable_interrupt(CommType, HwAddr, Port, Pin);
										ER->ER
									end;
								ER->ER
							end;
						ER->ER
					end;
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Enable interrupt on Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%% Output:
%%	 -
%% @end
-spec enable_interrupt(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin()) -> ok | {error, term()}.
%% ====================================================================
enable_interrupt(CommType, HwAddr, Port, Pin) ->
	do_interrupt_en_dis(CommType, HwAddr, Port, Pin, en).

%% ====================================================================
%% @doc
%% Disable interrupt on Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%% Output:
%%	 -
%% @end
-spec disable_interrupt(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin()) -> ok | {error, term()}.
%% ====================================================================
disable_interrupt(CommType, HwAddr, Port, Pin) ->
	do_interrupt_en_dis(CommType, HwAddr, Port, Pin, dis).

%% ====================================================================
%% @doc
%% Setup interrupt pin polarity
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 IntPol		: 1 = Active-high | 0 = Active-low.
%% Output:
%%	 -
%% @end
-spec setup_int_polarity(mcp23x17_comm_type(), hw_addr(), mcp23x17_int_pol()) -> ok | {error, term()}.
%% ====================================================================
setup_int_polarity(CommType, HwAddr, IntPol) ->
	{ok, IOCONRegValue} = read(CommType, HwAddr, ?IOCON_ADDR),
	NewIOCONRegValue = case IntPol of
						   ?MCP23X17_INT_POL_ACTIVE_HIGH ->
							   bit_operations:bit_set(IOCONRegValue, ?IOCON_INTPOL);
						   ?MCP23X17_INT_POL_ACTIVE_LOW ->
							   bit_operations:bit_clear(IOCONRegValue, ?IOCON_INTPOL)
								   end,
	write(CommType, HwAddr, IOCONRegValue, NewIOCONRegValue).

%% ====================================================================
%% @doc
%% Setup interrupt mirroring
%% Input:
%%	 CommType		: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr			: HW address of MCP chip
%%	 IntPinMirror	: 1 = The INT pins are internally connected
%%					  0 = The INT pins are not connected. INTA is associated with PortA and INTB is associated with PortB
%% Output:
%%	 -
%% @end
-spec setup_int_pin_mirror(mcp23x17_comm_type(), hw_addr(), mcp23x17_int_pin_mirror()) -> ok | {error, term()}.
%% ====================================================================
setup_int_pin_mirror(CommType, HwAddr, IntPinMirror) ->
	{ok, IOCONRegValue} = read(CommType, HwAddr, ?IOCON_ADDR),
	NewIOCONRegValue = case IntPinMirror of
						   ?MCP23X17_INT_PINS_MIRRORED ->
							   bit_operations:bit_set(IOCONRegValue, ?IOCON_MIRROR);
						   ?MCP23X17_INT_PINS_NOT_MIRRORED ->
							   bit_operations:bit_clear(IOCONRegValue, ?IOCON_MIRROR)
								   end,
	write(CommType, HwAddr, IOCONRegValue, NewIOCONRegValue).

%% ====================================================================
%% @doc
%% Read corresponding register if interrupt occurred
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%% Output:
%%	 RegVal		: value of INTFX_ADDR register
%% @end
-spec read_interrupt(mcp23x17_comm_type(), hw_addr(), mcp23x17_port()) -> {ok, reg_value()} | {error, term()}.
%% ====================================================================
read_interrupt(CommType, HwAddr, Port) ->
	%% Read interrupt source register
	{INTFRegAddr, GPIORegAddr, INTCAPRegAddr} = case Port of
													?MCP23X17_PORT_A ->
														{?INTFA_ADDR, ?GPIOA_ADDR, ?INTCAPA_ADDR};
													?MCP23X17_PORT_B ->
														{?INTFB_ADDR, ?GPIOB_ADDR, ?INTCAPB_ADDR}
												end,
	case read(CommType, HwAddr, INTFRegAddr) of
		{ok, INTFRegValue} ->
			%% Clear interrupt condition
			case read(CommType, HwAddr, GPIORegAddr) of
				{ok,_} ->
					case read(CommType, HwAddr, INTCAPRegAddr) of
						{ok,_} ->
							{ok, INTFRegValue};
						ER->ER
					end;
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Setup interrupt-on-change control of Port/Pin.
%% @end
-spec setup_interrupt_on_change_ctrl(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin(), mcp23x17_ioc_comp_val()) -> ok | {error, term()}.
%% ====================================================================
setup_interrupt_on_change_ctrl(CommType, HwAddr, Port, Pin, IntCtrl) ->
	%% Setup INTCON (Interrupt-On-Change Control) register.
	%% By default interrupt will occurred when IO Pin logic level
	%% matches with value of corresponding bit in DEFVAL register.
	INTCONRegAddr = case Port of
						?MCP23X17_PORT_A ->
							?INTCONA_ADDR;
						?MCP23X17_PORT_B ->
							?INTCONB_ADDR
					end,
	case read(CommType, HwAddr, INTCONRegAddr) of
		{ok, INTCONRegValue} ->
			NewINTCONRegValue = case IntCtrl of
									?MCP23X17_IOC_COMP_WITH_DEF_COMP_VALUE ->
										bit_operations:bit_set(INTCONRegValue, Pin);
									?MCP23X17_IOC_COMP_WITH_PREV_PIN_VALUE ->
										bit_operations:bit_clear(INTCONRegValue, Pin)
											   end,
			write(CommType, HwAddr, INTCONRegAddr, NewINTCONRegValue);
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Setup default compare register for interrupt-on-change event of Port/Pin.
%% @end
-spec setup_default_compare_reg_for_int_on_change(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin(), mcp23x17_def_comp_val()) -> ok | {error, term()}.
%% ====================================================================
setup_default_compare_reg_for_int_on_change(CommType, HwAddr, Port, Pin, DefComp) ->
	%% Setup default compare register for interrupt-on-change.
	DEFVALRegAddr = case Port of
				  ?MCP23X17_PORT_A ->
					  ?DEFVALA_ADDR;
				  ?MCP23X17_PORT_B ->
					  ?DEFVALB_ADDR
	end,
	case read(CommType, HwAddr, DEFVALRegAddr) of
		{ok, DEFVALRegValue} ->
			NewDEFVALRegValue = case DefComp of
									?MCP23X17_DEF_COMP_VALUE_HIGH ->
										bit_operations:bit_set(DEFVALRegValue, Pin);
									?MCP23X17_DEF_COMP_VALUE_LOW ->
										bit_operations:bit_clear(DEFVALRegValue, Pin)
											   end,
			write(CommType, HwAddr, DEFVALRegAddr, NewDEFVALRegValue);
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Setup IO direction of Port/Pin.
%% @end
-spec setup_io_direction(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin(), mcp23x17_io_dir()) -> ok | {error, term()}.
%% ====================================================================
setup_io_direction(CommType, HwAddr, Port, Pin, IODir) ->
	%% Skip this if the IO direction is laready set for the Pin.
	IsAlreadySet = case pin_rec_get(CommType, HwAddr, Port, Pin) of
		{ok, Rec} ->
			case Rec#rMCP23x17_PORT_PIN.direction of
				IODir ->
					%% IO direction of the Pin is already set.
					true;
				_-> {false, Rec}
			end;
		_-> {false, #rMCP23x17_PORT_PIN{pinId = {CommType, HwAddr, Port, Pin},
										direction = IODir}}
	end,
	
	case IsAlreadySet of
		true ->
			ok;
		{false, RecToBeUpdate}->
			%% Setup direction of Port/Pin to input
			IODirRegAddr = case Port of
						  ?MCP23X17_PORT_A ->
							  ?IODIRA_ADDR;
						  ?MCP23X17_PORT_B ->
							  ?IODIRB_ADDR
			end,
			case read(CommType, HwAddr, IODirRegAddr) of
				{ok, IODirRegValue} ->
					NewIODirRegValue = case IODir of
										   ?MCP23X17_IO_INPUT ->
											   bit_operations:bit_set(IODirRegValue, Pin);
										   ?MCP23X17_IO_OUTPUT ->
											   bit_operations:bit_clear(IODirRegValue, Pin)
												   end,
					case write(CommType, HwAddr, IODirRegAddr, NewIODirRegValue) of
						ok ->
							%% Update record in ETS
							pin_rec_set(RecToBeUpdate#rMCP23x17_PORT_PIN{direction = IODir}),
							ok;
						ER->ER
					end;
				ER->ER
			end
	end.

%% ====================================================================
%% @doc
%% Setup IO logical level of Port/Pin. So set to LOW or HIGH the pin.
%% Input:
%%	 CommType		: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr			: HW address of MCP chip
%%	 Port			: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin			: the Pin id of Port, <0..7>
%%	 LogicalLevel	: mcp23x17_io_logical_level()
%% Output:
%%	 ok | {error, Reason}
%% @end
-spec setup_io_logical_level(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin(), mcp23x17_io_logical_level()) -> ok | {error, term()}.
%% ====================================================================
setup_io_logical_level(CommType, HwAddr, Port, Pin, LogicalLevel) ->
	%% Setup IO direction to OUTPUT of Port/Pin first
	case setup_io_direction(CommType, HwAddr, Port, Pin, ?MCP23X17_IO_OUTPUT) of
		ok ->
			%% Set logical level of Port/Pin
			OLATRegAddr = case Port of
						  ?MCP23X17_PORT_A ->
							  ?OLATA_ADDR;
						  ?MCP23X17_PORT_B ->
							  ?OLATB_ADDR
			end,
			
			%% Set/Clear port/pin
			case read(CommType, HwAddr, OLATRegAddr) of
				{ok, OLATRegValue} ->
					NewOLATRegValue = case LogicalLevel of
										  ?MCP23X17_IO_LOGICAL_LOW ->
											  bit_operations:bit_clear(OLATRegValue, Pin);
										  ?MCP23X17_IO_LOGICAL_HIGH ->
											  bit_operations:bit_set(OLATRegValue, Pin)
												  end,
					write(CommType, HwAddr, OLATRegAddr, NewOLATRegValue);
				ER->ER
			end;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Get IO logical level of Port/Pin.
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%% Output:
%%	 {ok, mcp23x17_io_logical_level()} | {error, Reason}
%% @end
-spec get_io_logical_level(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin()) -> {ok, mcp23x17_io_logical_level()} | {error, term()}.
%% ====================================================================
get_io_logical_level(CommType, HwAddr, Port, Pin) ->
	OLATRegAddr = case Port of
				  ?MCP23X17_PORT_A ->
					  ?OLATA_ADDR;
				  ?MCP23X17_PORT_B ->
					  ?OLATB_ADDR
	end,
	
	%% Get port/pin
	case read(CommType, HwAddr, OLATRegAddr) of
		{ok, OLATRegValue} ->
			{ok, bit_operations:bit_test(OLATRegValue, Pin)};
		ER->ER
	end.
	
%% ====================================================================
%% @doc
%% Setup pull-up resistor of Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%%	 PullUpRes	: mcp23x17_pull_up_res_en() | mcp23x17_pull_up_res_dis()
%% Output:
%%	 -
%% @end
-spec setup_pull_up_resistor(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin(), mcp23x17_pull_up_res()) -> ok | {error, term()}.
%% ====================================================================
setup_pull_up_resistor(CommType, HwAddr, Port, Pin, PullUpRes) ->
	%% Setup Pull-Up resistor
	GPURegAddr = case Port of
				  ?MCP23X17_PORT_A ->
					  ?GPUA_ADDR;
				  ?MCP23X17_PORT_B ->
					  ?GPUB_ADDR
	end,
	case read(CommType, HwAddr, GPURegAddr) of
		{ok, GPURegValue} ->
			NewGPURegValue = case PullUpRes of
								 ?MCP23X17_PULL_UP_RES_ENABLED ->
									 bit_operations:bit_set(GPURegValue, Pin);
								 ?MCP23X17_PULL_UP_RES_DISABLED ->
									 bit_operations:bit_clear(GPURegValue, Pin)
										 end,
			write(CommType, HwAddr, GPURegAddr, NewGPURegValue);
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Setup input polarity of Port/Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%%	 IPol		: mcp23x17_in_pol()
%% Output:
%%	 -
%% @end
-spec setup_input_polarity(mcp23x17_comm_type(), hw_addr(), mcp23x17_port(), mcp23x17_pin(), mcp23x17_in_pol()) -> ok | {error, term()}.
%% ====================================================================
setup_input_polarity(CommType, HwAddr, Port, Pin, IPol) ->
	%% Setup input polarity
	IPolRegAddr = case Port of
				  ?MCP23X17_PORT_A ->
					  ?IPOLA_ADDR;
				  ?MCP23X17_PORT_B ->
					  ?IPOLB_ADDR
	end,
	case read(CommType, HwAddr, IPolRegAddr) of
		{ok, IPolRegValue} ->
			NewIPolRegValue = case IPol of
				?MCP23X17_INPUT_POL_SAME_OF_PIN_LOGIC ->
					bit_operations:bit_clear(IPolRegValue, Pin);
				?MCP23X17_INPUT_POL_OPPOSITE_OF_PIN_LOGIC ->
					bit_operations:bit_set(IPolRegValue, Pin)
							  end,
			write(CommType, HwAddr, IPolRegAddr, NewIPolRegValue);
		ER->ER
	end.

%% ====================================================================
%% Give the record (rMCP23x17_PORT_PIN) of PIN.
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%% Output:
%%	{ok, Rec} | {error, Reason}
%% ====================================================================
pin_rec_get(CommType, HwAddr, Port, Pin) ->
	%% Start server if not yet started.
	start_link(),
	
	case gen_server:call(?SERVER, {pin_rec_get, CommType, HwAddr, Port, Pin}, 1000) of
		{ok,Rec} when is_record(Rec, rMCP23x17_PORT_PIN) ->
			{ok,Rec};
		ER->ER
	end.

%% ====================================================================
%% Set Record of Pin in ETS table.
%% Input:
%%	Rec	:	record of rMCP23x17_PORT_PIN
%% Output:
%%	{ok, Rec} | {error, Reason}
%% ====================================================================
pin_rec_set(Rec) when is_record(Rec, rMCP23x17_PORT_PIN)->
	%% Start server if not yet started.
	start_link(),
	
	case gen_server:call(?SERVER, {pin_rec_set, Rec}, 1000) of
		ok ->
			ok;
		ER->ER
	end.

%% ====================================================================
%% @doc
%% Stop I2C driver.
%% @end
-spec i2c_driver_stop(mcp23x17_comm_type(), hw_addr()) -> ok | {error, term()}.
%% ====================================================================
i2c_driver_stop(CommType, HwAddr) ->
	ale_handler:i2c_stop(get_comm_devicename(CommType), HwAddr).

%% ====================================================================
%% @doc
%% Stop SPI driver.
%% @end
-spec spi_driver_stop(mcp23x17_comm_type()) -> ok | {error, term()}.
%% ====================================================================
spi_driver_stop(CommType) ->
	ale_handler:spi_stop(get_comm_devicename(CommType)).

%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	ets:new(?MCP23X17_PIN_TABLE, [ordered_set,public,named_table,{keypos,?MCP23X17_PIN_TABLE_KEYPOS}]),
	{ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({pin_rec_get, CommType, HwAddr, Port, Pin}, _From, State) ->
	%% Find pin in the ETS table
	Reply = case ets:lookup(?MCP23X17_PIN_TABLE, {CommType, HwAddr, Port, Pin}) of
				[Rec] when is_record(Rec, rMCP23x17_PORT_PIN) ->
					{ok, Rec};
				_->	{error, "Pin does not found."}
			end,
	{reply, Reply, State};

handle_call({pin_rec_set, Rec}, _From, State) ->
	Reply = case ets:insert(?MCP23X17_PIN_TABLE, Rec) of
				true ->
					ok;
				ER ->
					{error, ER}
			end,
	{reply, Reply, State};

handle_call({stop}, _From, State) ->
	{stop, normal, ok, State};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
	{noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
	{noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
	ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% @doc
%% Read MCP register
%% Input:
%%	 CommType	: type of serial communication. It can be MCP23X17_COMM_TYPE_SPI0 | MCP23X17_COMM_TYPE_I2C1
%%	 HwAddr		: HW address of MCP chip
%%	 RegAddr	: address of MCP register to be read
%% Output:
%%	 RegValue : value of register
%% @end
-spec read(mcp23x17_comm_type(), hw_addr(), reg_addr()) -> {ok, reg_value()} | {error, term()}.
%% ====================================================================
read(CommType, HwAddr, RegAddr) ->
	case CommType of
		?MCP23X17_COMM_TYPE_SPI0 ->
			%% CS leg is controlled by Raspberry Pi independently.
			case ale_handler:spi_transfer(get_comm_devicename(CommType), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_read(HwAddr), RegAddr, ?DUMMY_BYTE_FOR_READ])) of
				{ok, <<_,_,RegValue>>} ->
					{ok, RegValue};
				ER-> ER
			end;
		
		{?MCP23X17_COMM_TYPE_SPI0, SelectSPIByCS, UnselectSPIByCS} ->
			%% CS leg is controlled by SelectSPIByCS and UnselectSPIByCS MFAs.
			
			%% Select SPI device by CS
			spi_cs(SelectSPIByCS),
			
			case ale_handler:spi_transfer(get_comm_devicename(?MCP23X17_COMM_TYPE_SPI0), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_read(HwAddr), RegAddr, ?DUMMY_BYTE_FOR_READ])) of
				{ok, <<_,_,RegValue>>} ->
					%% Unselect SPI device by CS
					spi_cs(UnselectSPIByCS),
					
					{ok, RegValue};
				ER-> 
					%% Unselect SPI device by CS
					spi_cs(UnselectSPIByCS),
					ER
			end;
		
		?MCP23X17_COMM_TYPE_I2C1 ->
			ale_handler:i2c_write(get_comm_devicename(CommType), HwAddr, erlang:list_to_binary([RegAddr])),
			timer:sleep(10),
			case ale_handler:i2c_read(get_comm_devicename(CommType), HwAddr, 1) of
				{ok, <<Data>>} ->
					{ok, Data};
				ER->ER
			end
	end.

%% ====================================================================
%% @doc
%% Write MCP register
%% Input:
%%	 CommType : type of serial communication. It can be MCP23X17_COMM_TYPE_SPI0 | MCP23X17_COMM_TYPE_I2C1
%%	 HwAddr	  : HW address of MCP chip
%%	 RegAddr  : address of MCP register to be read
%%	 RegValue : value of register
%% Output:
%%	 -
%% @end
-spec write(mcp23x17_comm_type(), hw_addr(), reg_addr(), reg_value()) -> ok | {error, term()}.
%% ====================================================================
write(CommType, HwAddr, RegAddr, RegValue) ->
	case CommType of
		?MCP23X17_COMM_TYPE_SPI0 ->
			%% CS leg is controlled by Raspberry Pi independently.
			case ale_handler:spi_transfer(get_comm_devicename(CommType), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_write(HwAddr), RegAddr, RegValue])) of
				{error, Reason} ->
					{error, Reason};
				_->	ok
			end;
		
		{?MCP23X17_COMM_TYPE_SPI0, SelectSPIByCS, UnselectSPIByCS} ->
			%% CS leg is controlled by SelectSPIByCS and UnselectSPIByCS MFAs.
			
			%% Select SPI device by CS
			spi_cs(SelectSPIByCS),
			
			case ale_handler:spi_transfer(get_comm_devicename(?MCP23X17_COMM_TYPE_SPI0), ?SPI_DEVICE_DEFAULT_OPTIONS, erlang:list_to_binary([set_hwaddr_for_write(HwAddr), RegAddr, RegValue])) of
				{error, Reason} ->
					%% Unselect SPI device by CS
					spi_cs(UnselectSPIByCS),
					
					{error, Reason};
				_->	
					%% Unselect SPI device by CS
					spi_cs(UnselectSPIByCS),
					ok
			end;
		
		?MCP23X17_COMM_TYPE_I2C1 ->
			ale_handler:i2c_write(get_comm_devicename(CommType), HwAddr, erlang:list_to_binary([RegAddr, RegValue]))
	end.

%% ====================================================================
%% Give the I2C or SPI device name.
%% Input:
%%	CommType	:	atom, type of serial communication. It can be MCP23X17_COMM_TYPE_SPI0 | MCP23X17_COMM_TYPE_I2C1
%% Output:
%%	CommChData	:	string
%% ====================================================================
get_comm_devicename(CommType) ->
	case CommType of
		?MCP23X17_COMM_TYPE_I2C1 ->
			?I2C_DEVICE_CH1_NAME;
		?MCP23X17_COMM_TYPE_SPI0 ->
			?SPI_DEVICE_CH0_NAME
	end.

%% ====================================================================
%% Enable/Disable Interrupt on the Pin
%% Input:
%%	 CommType	: type of serial communication. It can be MCP_COMM_TYPE_SPI | MCP_COMM_TYPE_I2C
%%	 HwAddr		: HW address of MCP chip
%%	 Port		: possible value can be MCP23X17_PORT_A | MCP23X17_PORT_B
%%	 Pin		: the Pin id of Port, <0..7>
%%	 EnDis		: en | dis
%% Output:
%%	ok | {error, Reason}
%% ====================================================================
do_interrupt_en_dis(CommType, HwAddr, Port, Pin, EnDis) ->
	GPINTENRegAddr = case Port of
						 ?MCP23X17_PORT_A ->
							 ?GPINTENA_ADDR;
						 ?MCP23X17_PORT_B ->
							 ?GPINTENB_ADDR
					end,
	case read(CommType, HwAddr, GPINTENRegAddr) of
		{ok, GPINTENRegValue} ->
			NewGPINTENRegValue = case EnDis of
									 en ->
										 bit_operations:bit_set(GPINTENRegValue, Pin);
									 dis ->
										 bit_operations:bit_clear(GPINTENRegValue, Pin)
												end,
			write(CommType, HwAddr, GPINTENRegAddr, NewGPINTENRegValue);
		ER->ER
	end.

%% ====================================================================
%% Manipulate HW address for Read/Write operation. According to the
%% protocol description, this is required only for I2C, but the I2C driver
%% does this, so here in MCP23x17 level no need to do.
%% This is not needed for SPI protocol, BUT this is required for MCP23x17,
%% according to its documentation. This means, the 1st bit on HW address should be
%% changed accordingly.
%% ====================================================================
set_hwaddr_for_read(HwAddr) ->
	%% Shift left the address with 1 bit first.
	NewHwAddr = HwAddr bsl 1,
	
	%% Set 1st bit of NewHwAddr
	bit_operations:bit_set(NewHwAddr, 0).
	
set_hwaddr_for_write(HwAddr) ->
	%% Shift left the address with 1 bit first.
	NewHwAddr = HwAddr bsl 1,
	
	%% Set 1st bit of NewHwAddr
	bit_operations:bit_clear(NewHwAddr, 0).

%% ====================================================================
%% Select/Unselect SPI by CS leg. This is used when NOT the Pi handles
%% the CS independently, but other logic network controls that.
%% Input:
%%		CS_MFA	:	MFA tuple for set/clear CS
%% Output:
%%		ok | {error, Reason}
%% ====================================================================
spi_cs({M,F,A}) ->
	erlang:apply(M, F, A).


