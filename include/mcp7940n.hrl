%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This module contains definitions for MCP7940n RTC device.
%% @end

-ifndef(MCP7940N_HRL).
-define(MCP7940N_HRL,true).


%% ====================================================================
%% Includes
%% ====================================================================
-include("ale_common.hrl").

%% ====================================================================
%% Notifications
%% ====================================================================
-define(NOTIFICATION_PWR_IS_BACK, main_power_is_back).
-define(NOTIFICATION_PWR_IS_LOST, main_power_is_lost).

%% ====================================================================
%% This is the I2C address of external MCP7940N clock chip.
%% This is 7bit address!!
%% ====================================================================
-define(RTC_ADDRESS, 2#1101111).

%% ====================================================================
%% Communicatoin device name to be used for data transfer
%% ====================================================================
-define(RTC_COMMUNICATION_DEVICENAME, ?I2C_DEVICE_CH1_NAME).

%% ====================================================================
%% Define BASE_YEAR parameter. This needs for get/set year value in RTC device
%% because RTC device handle years between 0-99 only.
%% ====================================================================
-define(BASE_YEAR, 2000).

%% ====================================================================
%% Define for alarm ids.
%% ====================================================================
-define(RTC_ALARM_0_ID, 0).
-define(RTC_ALARM_1_ID, 1).

%% ====================================================================
%% Bit definitions
%% ====================================================================
-define(RTC_12H_Mode, 1).
-define(RTC_24H_Mode, 0).

-define(RTC_AM_Mode, 0).
-define(RTC_PM_Mode, 1).

%% ====================================================================
%% Register addresses of RTC
%% Structure of register:
%% -record(NAME, {				
%% 				address = <ADDRESS OF REGISTER>,
%% 				bit_x = bit_parameter_record}).
%% where bit_parameter_record
-record(bitParam, {	
					value,			%% {Min,Max} | [ListOfPossibleValues]
					mask,			%% mask of the bit in the byte
					doshiftvalue	%% boolean
									%% If value is tuple, it must be true. This means the new value of bit must shift 
									%% according to its mask before set, and after read from the RTC device. This is
									%% because the {Min,Max} values in the tuple gives the "normal" possible values of the bit
									%% and this value need to shift according to the mask of the byte. 
  					}).
%% ====================================================================

%% ====================================================================
%% SECOND REGISTER
%% ====================================================================
-define(RTC_SECOND_BIT_ST_EN,				1).
-define(RTC_SECOND_BIT_ST_DIS,				0).
-define(RTC_SECOND_BIT_ST_MASK,				2#10000000).

-define(RTC_SECOND_BIT_SECTEN,				{0,5}).
-define(RTC_SECOND_BIT_SECTEN_MASK,			2#01110000).

-define(RTC_SECOND_BIT_SECONE,				{0,9}).
-define(RTC_SECOND_BIT_SECONE_MASK,			2#00001111).
-record(rtcSecondReg, {
				   address = 16#00,
				   bit_st = #bitParam{value = [?RTC_SECOND_BIT_ST_EN, ?RTC_SECOND_BIT_ST_DIS], mask = ?RTC_SECOND_BIT_ST_MASK, doshiftvalue = true},
				   bit_secTen = #bitParam{value = ?RTC_SECOND_BIT_SECTEN, mask = ?RTC_SECOND_BIT_SECTEN_MASK, doshiftvalue = true},
				   bit_secOne = #bitParam{value = ?RTC_SECOND_BIT_SECONE, mask = ?RTC_SECOND_BIT_SECONE_MASK, doshiftvalue = true}
				  }).

%% ====================================================================
%% MINUTE REGISTER
%% ====================================================================
-define(RTC_MINUTE_BIT_MINTEN,		{0,5}).
-define(RTC_MINUTE_BIT_MINTEN_MASK,	2#01110000).

-define(RTC_MINUTE_BIT_MINONE,		{0,9}).
-define(RTC_MINUTE_BIT_MINONE_MASK,	2#00001111).
-record(rtcMinuteReg, {
				   address = 16#01,
				   bit_minTen = #bitParam{value = ?RTC_MINUTE_BIT_MINTEN, mask = ?RTC_MINUTE_BIT_MINTEN_MASK, doshiftvalue = true},
				   bit_minOne = #bitParam{value = ?RTC_MINUTE_BIT_MINONE, mask = ?RTC_MINUTE_BIT_MINONE_MASK, doshiftvalue = true}
				  }).

%% ====================================================================
%% HOUR REGISTER
%% ====================================================================
-define(RTC_HOUR_BIT_TIME_FORMAT_12H,		1).
-define(RTC_HOUR_BIT_TIME_FORMAT_24H,		0).
-define(RTC_HOUR_BIT_TIME_FORMAT_MASK,		2#01000000).

%% If 12H format
-define(RTC_HOUR_BIT_12H_AMPM_IND_PM,	1).
-define(RTC_HOUR_BIT_12H_AMPM_IND_AM,	0).
-define(RTC_HOUR_BIT_12H_AMPM_IND_MASK,	2#00100000).

-define(RTC_HOUR_BIT_12H_HRTEN0,		{0,1}).
-define(RTC_HOUR_BIT_12H_HRTEN0_MASK,	2#00010000).

-define(RTC_HOUR_BIT_12H_HRONE,			{0,9}).
-define(RTC_HOUR_BIT_12H_HRONE_MASK,	2#00001111).

%% If 24H format
-define(RTC_HOUR_BIT_24H_HRTEN,			{0,2}).
-define(RTC_HOUR_BIT_24H_HRTEN_MASK,	2#00110000).

-define(RTC_HOUR_BIT_24H_HRONE,			{0,9}).
-define(RTC_HOUR_BIT_24H_HRONE_MASK,	2#00001111).

-record(rtcHourReg, {
				 address = 16#02,
				 bit_timeFormat = #bitParam{value = [?RTC_HOUR_BIT_TIME_FORMAT_12H, ?RTC_HOUR_BIT_TIME_FORMAT_24H], mask = ?RTC_HOUR_BIT_TIME_FORMAT_MASK, doshiftvalue = true},
				 
				 %% 12H mode
				 bit_12h_ampm = #bitParam{value = [?RTC_HOUR_BIT_12H_AMPM_IND_PM, ?RTC_HOUR_BIT_12H_AMPM_IND_AM], mask = ?RTC_HOUR_BIT_12H_AMPM_IND_MASK, doshiftvalue = true},
				 bit_12h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_12H_HRTEN0, mask = ?RTC_HOUR_BIT_12H_HRTEN0_MASK, doshiftvalue = true},
				 bit_12h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_12H_HRONE, mask = ?RTC_HOUR_BIT_12H_HRONE_MASK, doshiftvalue = true},
				 
				 %% 24H mode
				 bit_24h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_24H_HRTEN, mask = ?RTC_HOUR_BIT_24H_HRTEN_MASK, doshiftvalue = true},
				 bit_24h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_24H_HRONE, mask = ?RTC_HOUR_BIT_24H_HRONE_MASK, doshiftvalue = true}
				}).

%% ====================================================================
%% WKDAY REGISTER
%% ====================================================================
-define(RTC_WKDAY_BIT_OSCRUN_EN,			1).
-define(RTC_WKDAY_BIT_OSCRUN_DIS,			0).
-define(RTC_WKDAY_BIT_OSCRUN_MASK,			2#00100000).

-define(RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST,	1).
-define(RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST,0).
-define(RTC_WKDAY_BIT_PWRFAIL_MASK,				2#00010000).

-define(RTC_WKDAY_BIT_VBATEN_EN,			1).
-define(RTC_WKDAY_BIT_VBATEN_DIS,			0).
-define(RTC_WKDAY_BIT_VBATEN_MAS,			2#00001000).

-define(RTC_WKDAY_BIT_WKDAY,				{1,7}).
-define(RTC_WKDAY_BIT_WKDAY_MASK,			2#00000111).
-record(rtcWkDayReg, {
					  address = 16#03,
					  bit_oscRun = #bitParam{value = [?RTC_WKDAY_BIT_OSCRUN_EN, ?RTC_WKDAY_BIT_OSCRUN_DIS], mask = ?RTC_WKDAY_BIT_OSCRUN_MASK, doshiftvalue = true},
					  bit_pwrFail = #bitParam{value = [?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST, ?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST], mask = ?RTC_WKDAY_BIT_PWRFAIL_MASK, doshiftvalue = true},
					  bit_vBatEn = #bitParam{value = [?RTC_WKDAY_BIT_VBATEN_EN, ?RTC_WKDAY_BIT_VBATEN_DIS], mask = ?RTC_WKDAY_BIT_VBATEN_MAS, doshiftvalue = true},
					  bit_wDay = #bitParam{value = ?RTC_WKDAY_BIT_WKDAY, mask = ?RTC_WKDAY_BIT_WKDAY_MASK, doshiftvalue = true}
					 }).

%% ====================================================================
%% DATE REGISTER
%% ====================================================================
-define(RTC_DATE_BIT_DATETEN,		{0,3}).
-define(RTC_DATE_BIT_DATETEN_MASK,	2#00110000).

-define(RTC_DATE_BIT_DATEONE,		{0,9}).
-define(RTC_DATE_BIT_DATEONE_MASK,	2#00001111).
-record(rtcDateReg, {
					 address = 16#04,
					 bit_dateTen = #bitParam{value = ?RTC_DATE_BIT_DATETEN, mask = ?RTC_DATE_BIT_DATETEN_MASK, doshiftvalue = true},
					 bit_dateOne = #bitParam{value = ?RTC_DATE_BIT_DATEONE, mask = ?RTC_DATE_BIT_DATEONE_MASK, doshiftvalue = true}
					}).

%% ====================================================================
%% MONTH REGISTER
%% ====================================================================
-define(RTC_MONTH_BIT_LPYR_LEAPY,	1).
-define(RTC_MONTH_BIT_LPYR_NOLEAPY,	0).
-define(RTC_MONTH_BIT_LPYR_MASK,	2#00100000).

-define(RTC_MONTH_BIT_MTHTEN0,		{0,1}).
-define(RTC_MONTH_BIT_MTHTEN0_MASK,	2#00010000).

-define(RTC_MONTH_BIT_MTHONE,		{0,9}).
-define(RTC_MONTH_BIT_MTHONE_MASK,	2#00001111).
-record(rtcMonthReg, {
					  address = 16#05,
					  bit_lpyr = #bitParam{value = [?RTC_MONTH_BIT_LPYR_LEAPY, ?RTC_MONTH_BIT_LPYR_NOLEAPY], mask = ?RTC_MONTH_BIT_LPYR_MASK, doshiftvalue = true},
					  bit_monthTen = #bitParam{value = ?RTC_MONTH_BIT_MTHTEN0, mask = ?RTC_MONTH_BIT_MTHTEN0_MASK, doshiftvalue = true},
					  bit_monthOne = #bitParam{value = ?RTC_MONTH_BIT_MTHONE, mask = ?RTC_MONTH_BIT_MTHONE_MASK, doshiftvalue = true}
					  }).

%% ====================================================================
%% YEAR REGISTER
%% ====================================================================
-define(RTC_YEAR_BIT_YEARTEN,		{0,9}).
-define(RTC_YEAR_BIT_YEARTEN_MASK,	2#11110000).

-define(RTC_YEAR_BIT_YEARONE,		{0,9}).
-define(RTC_YEAR_BIT_YEARONE_MASK,	2#00001111).

-record(rtcYearReg, {
					 address = 16#06,
					 bit_yearTen = #bitParam{value = ?RTC_YEAR_BIT_YEARTEN, mask = ?RTC_YEAR_BIT_YEARTEN_MASK, doshiftvalue = true},
					 bit_yearOne = #bitParam{value = ?RTC_YEAR_BIT_YEARONE, mask = ?RTC_YEAR_BIT_YEARONE_MASK, doshiftvalue = true}
					 }).

%% ====================================================================
%% ALARM - SECOND REGISTERS
%% ====================================================================
-define(RTC_A0_SEC_ADDR,		16#0A).
-define(RTC_A1_SEC_ADDR,		16#11).
-record(rtcAlm0SecReg, {
					   address = ?RTC_A0_SEC_ADDR,
					   bit_secTen = #bitParam{value = ?RTC_SECOND_BIT_SECTEN, mask = ?RTC_SECOND_BIT_SECTEN_MASK, doshiftvalue = true},
					   bit_secOne = #bitParam{value = ?RTC_SECOND_BIT_SECONE, mask = ?RTC_SECOND_BIT_SECONE_MASK, doshiftvalue = true}
					  }).

-record(rtcAlm1SecReg, {
					   address = ?RTC_A1_SEC_ADDR,
					   bit_secTen = #bitParam{value = ?RTC_SECOND_BIT_SECTEN, mask = ?RTC_SECOND_BIT_SECTEN_MASK, doshiftvalue = true},
					   bit_secOne = #bitParam{value = ?RTC_SECOND_BIT_SECONE, mask = ?RTC_SECOND_BIT_SECONE_MASK, doshiftvalue = true}
					  }).

%% ====================================================================
%% ALARM - MINUTE REGISTERS
%% ====================================================================
-define(RTC_A0_MIN_ADDR,		16#0B).
-define(RTC_A1_MIN_ADDR,		16#12).
-record(rtcAlm0MinReg, {
					   address = ?RTC_A0_MIN_ADDR,
					   bit_minTen = #bitParam{value = ?RTC_MINUTE_BIT_MINTEN, mask = ?RTC_MINUTE_BIT_MINTEN_MASK, doshiftvalue = true},
					   bit_minOne = #bitParam{value = ?RTC_MINUTE_BIT_MINONE, mask = ?RTC_MINUTE_BIT_MINONE_MASK, doshiftvalue = true}
					  }).

-record(rtcAlm1MinReg, {
					   address = ?RTC_A1_MIN_ADDR,
					   bit_minTen = #bitParam{value = ?RTC_MINUTE_BIT_MINTEN, mask = ?RTC_MINUTE_BIT_MINTEN_MASK, doshiftvalue = true},
					   bit_minOne = #bitParam{value = ?RTC_MINUTE_BIT_MINONE, mask = ?RTC_MINUTE_BIT_MINONE_MASK, doshiftvalue = true}
					  }).

%% ====================================================================
%% ALARM - HOUR REGISTERS
%% ====================================================================
-define(RTC_A0_HOUR_ADDR,		16#0C).
-define(RTC_A1_HOUR_ADDR,		16#13).
-record(rtcAlm0HourReg, {
				 address = ?RTC_A0_HOUR_ADDR,
				 bit_timeFormat = #bitParam{value = [?RTC_HOUR_BIT_TIME_FORMAT_12H, ?RTC_HOUR_BIT_TIME_FORMAT_24H], mask = ?RTC_HOUR_BIT_TIME_FORMAT_MASK, doshiftvalue = true},
				 
				 %% 12H mode
				 bit_12h_ampm = #bitParam{value = [?RTC_HOUR_BIT_12H_AMPM_IND_PM, ?RTC_HOUR_BIT_12H_AMPM_IND_AM], mask = ?RTC_HOUR_BIT_12H_AMPM_IND_MASK, doshiftvalue = true},
				 bit_12h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_12H_HRTEN0, mask = ?RTC_HOUR_BIT_12H_HRTEN0_MASK, doshiftvalue = true},
				 bit_12h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_12H_HRONE, mask = ?RTC_HOUR_BIT_12H_HRONE_MASK, doshiftvalue = true},
				 
				 %% 24H mode
				 bit_24h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_24H_HRTEN, mask = ?RTC_HOUR_BIT_24H_HRTEN_MASK, doshiftvalue = true},
				 bit_24h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_24H_HRONE, mask = ?RTC_HOUR_BIT_24H_HRONE_MASK, doshiftvalue = true}
				}).

-record(rtcAlm1HourReg, {
				 address = ?RTC_A1_HOUR_ADDR,
				 bit_timeFormat = #bitParam{value = [?RTC_HOUR_BIT_TIME_FORMAT_12H, ?RTC_HOUR_BIT_TIME_FORMAT_24H], mask = ?RTC_HOUR_BIT_TIME_FORMAT_MASK, doshiftvalue = true},
				 
				 %% 12H mode
				 bit_12h_ampm = #bitParam{value = [?RTC_HOUR_BIT_12H_AMPM_IND_PM, ?RTC_HOUR_BIT_12H_AMPM_IND_AM], mask = ?RTC_HOUR_BIT_12H_AMPM_IND_MASK, doshiftvalue = true},
				 bit_12h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_12H_HRTEN0, mask = ?RTC_HOUR_BIT_12H_HRTEN0_MASK, doshiftvalue = true},
				 bit_12h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_12H_HRONE, mask = ?RTC_HOUR_BIT_12H_HRONE_MASK, doshiftvalue = true},
				 
				 %% 24H mode
				 bit_24h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_24H_HRTEN, mask = ?RTC_HOUR_BIT_24H_HRTEN_MASK, doshiftvalue = true},
				 bit_24h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_24H_HRONE, mask = ?RTC_HOUR_BIT_24H_HRONE_MASK, doshiftvalue = true}
				}).

%% ====================================================================
%% ALARM - WKDAY REGISTERS
%% ====================================================================
-define(RTC_A0_WKDAY_ADDR,		16#0D).
-define(RTC_A1_WKDAY_ADDR,		16#14).

-define(RTC_ALMxWKDAY_BIT_ALMPOL_HIGH,	1).
-define(RTC_ALMxWKDAY_BIT_ALMPOL_LOW,	0).
-define(RTC_ALMxWKDAY_BIT_ALMPOL_MASK,	2#10000000).

-define(RTC_ALMxWKDAY_BIT_ALMxMASK_SEC_MATCH,	2#000).
-define(RTC_ALMxWKDAY_BIT_ALMxMASK_MIN_MATCH,	2#001).
-define(RTC_ALMxWKDAY_BIT_ALMxMASK_HOUR_MATCH,	2#010).
-define(RTC_ALMxWKDAY_BIT_ALMxMASK_WDAY_MATCH,	2#011).
-define(RTC_ALMxWKDAY_BIT_ALMxMASK_ALL_MATCH,	2#111).
-define(RTC_ALMxWKDAY_BIT_ALMxMASK_MASK,		2#01110000).

-define(RTC_ALMxWKDAY_BIT_ALMxIF_SET,		1).
-define(RTC_ALMxWKDAY_BIT_ALMxIF_CLEAR,		0).
-define(RTC_ALMxWKDAY_BIT_ALMxIF_MASK,		2#00001000).

-define(RTC_ALMxWKDAY_BIT_WKDAY,		{1,7}).
-define(RTC_ALMxWKDAY_BIT_WKDAY_MASK,	2#00000111).

-record(rtcAlm0WDayReg, {
						 address = ?RTC_A0_WKDAY_ADDR,
						 bit_almPol = #bitParam{value = {?RTC_ALMxWKDAY_BIT_ALMPOL_LOW, ?RTC_ALMxWKDAY_BIT_ALMPOL_HIGH}, mask = ?RTC_ALMxWKDAY_BIT_ALMPOL_MASK, doshiftvalue = true},
						 bit_almMask = #bitParam{value = [?RTC_ALMxWKDAY_BIT_ALMxMASK_SEC_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_MIN_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_HOUR_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_WDAY_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_ALL_MATCH], mask = ?RTC_ALMxWKDAY_BIT_ALMxMASK_MASK, doshiftvalue = true},
						 bit_almIf = #bitParam{value = [?RTC_ALMxWKDAY_BIT_ALMxIF_CLEAR, ?RTC_ALMxWKDAY_BIT_ALMxIF_SET], mask = ?RTC_ALMxWKDAY_BIT_ALMxIF_MASK, doshiftvalue = true},
						 bit_wDay = #bitParam{value = ?RTC_ALMxWKDAY_BIT_WKDAY, mask = ?RTC_ALMxWKDAY_BIT_WKDAY_MASK, doshiftvalue = true}
						 }).

-record(rtcAlm1WDayReg, {
						 address = ?RTC_A1_WKDAY_ADDR,
						 bit_almPol = #bitParam{value = {?RTC_ALMxWKDAY_BIT_ALMPOL_LOW, ?RTC_ALMxWKDAY_BIT_ALMPOL_HIGH}, mask = ?RTC_ALMxWKDAY_BIT_ALMPOL_MASK, doshiftvalue = true},
						 bit_almMask = #bitParam{value = [?RTC_ALMxWKDAY_BIT_ALMxMASK_SEC_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_MIN_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_HOUR_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_WDAY_MATCH,
														  ?RTC_ALMxWKDAY_BIT_ALMxMASK_ALL_MATCH], mask = ?RTC_ALMxWKDAY_BIT_ALMxMASK_MASK, doshiftvalue = true},
						 bit_almIf = #bitParam{value = [?RTC_ALMxWKDAY_BIT_ALMxIF_CLEAR, ?RTC_ALMxWKDAY_BIT_ALMxIF_SET], mask = ?RTC_ALMxWKDAY_BIT_ALMxIF_MASK, doshiftvalue = true},
						 bit_wDay = #bitParam{value = ?RTC_ALMxWKDAY_BIT_WKDAY, mask = ?RTC_ALMxWKDAY_BIT_WKDAY_MASK, doshiftvalue = true}
						 }).

%% ====================================================================
%% ALARM - DATE REGISTERS
%% ====================================================================
-define(RTC_A0_DATE_ADDR,	16#0E).
-define(RTC_A1_DATE_ADDR,	16#15).
-record(rtcAlm0DateReg, {
						address = ?RTC_A0_DATE_ADDR,
						bit_dateTen = #bitParam{value = ?RTC_DATE_BIT_DATETEN, mask = ?RTC_DATE_BIT_DATETEN_MASK, doshiftvalue = true},
						bit_dateOne = #bitParam{value = ?RTC_DATE_BIT_DATEONE, mask = ?RTC_DATE_BIT_DATEONE_MASK, doshiftvalue = true}
					}).
-record(rtcAlm1DateReg, {
						address = ?RTC_A1_DATE_ADDR,
						bit_dateTen = #bitParam{value = ?RTC_DATE_BIT_DATETEN, mask = ?RTC_DATE_BIT_DATETEN_MASK, doshiftvalue = true},
						bit_dateOne = #bitParam{value = ?RTC_DATE_BIT_DATEONE, mask = ?RTC_DATE_BIT_DATEONE_MASK, doshiftvalue = true}
					}).

%% ====================================================================
%% ALARM - MONTH REGISTERS
%% ====================================================================
-define(RTC_A0_MONTH_ADDR,	16#0F).
-define(RTC_A1_MONTH_ADDR,	16#16).
-record(rtcAlm0MonthReg, {
			address = ?RTC_A0_MONTH_ADDR,
			bit_monthTen = #bitParam{value = ?RTC_MONTH_BIT_MTHTEN0, mask = ?RTC_MONTH_BIT_MTHTEN0_MASK, doshiftvalue = true},
			bit_monthOne = #bitParam{value = ?RTC_MONTH_BIT_MTHONE, mask = ?RTC_MONTH_BIT_MTHONE_MASK, doshiftvalue = true}
					  }).

-record(rtcAlm1MonthReg, {
			address = ?RTC_A1_MONTH_ADDR,
			bit_monthTen = #bitParam{value = ?RTC_MONTH_BIT_MTHTEN0, mask = ?RTC_MONTH_BIT_MTHTEN0_MASK, doshiftvalue = true},
			bit_monthOne = #bitParam{value = ?RTC_MONTH_BIT_MTHONE, mask = ?RTC_MONTH_BIT_MTHONE_MASK, doshiftvalue = true}
					  }).

%% ====================================================================
%% CONTROL REGISTER
%% ====================================================================
%% bit 7, OUT: Logic Level for General Purpose Output bit
-define(RTC_CTRL_BIT_OUT_MFP_HIGH,		1).
-define(RTC_CTRL_BIT_OUT_MFP_LOW,		0).
-define(RTC_CTRL_BIT_OUT_MFP_MASK,		2#10000000).

%% bit 6: Square Wave Output Enable bit
-define(RTC_CTRL_BIT_SQWEN_EN,			1).
-define(RTC_CTRL_BIT_SQWEN_DIS,			0).
-define(RTC_CTRL_BIT_SQWEN_MASK,		2#01000000).

%% bit 5,4: Alarm 1,0 enabled/disabled
-define(RTC_CTRL_BIT_ALM_Ax_EN,			1).
-define(RTC_CTRL_BIT_ALM_Ax_DIS,		0).
-define(RTC_CTRL_BIT_ALM_A1_MASK,		2#00100000).
-define(RTC_CTRL_BIT_ALM_A0_MASK,		2#00010000).

%% bit 3: External Oscillator Input bit
-define(RTC_CTRL_BIT_EXTOSC_EN,			1).
-define(RTC_CTRL_BIT_EXTOSC_DIS,		0).
-define(RTC_CTRL_BIT_EXTOSC_MASK,		2#00001000).

%% bit 2: Coarse Trim Mode Enable bit
-define(RTC_CTRL_BIT_CRSTRIM_EN,		1).
-define(RTC_CTRL_BIT_CRSTRIM_DIS,		0).
-define(RTC_CTRL_BIT_CRSTRIM_MASK,		2#00000100).

%% bit 1,0: Bitmask for RSx bits in CONTROL register
-define(RTC_CTRL_BIT_SQWFS_1Hz,			2#00).
-define(RTC_CTRL_BIT_SQWFS_4069Hz,		2#01).
-define(RTC_CTRL_BIT_SQWFS_8192Hz,		2#10).
-define(RTC_CTRL_BIT_SQWFS_32768Hz,		2#11).
-define(RTC_CTRL_BIT_SQWFS_MASK,		2#00000011).

-record(rtcControlReg, {
					address = 16#07,
					bit_out = #bitParam{value = [?RTC_CTRL_BIT_OUT_MFP_HIGH, ?RTC_CTRL_BIT_OUT_MFP_LOW], mask = ?RTC_CTRL_BIT_OUT_MFP_MASK, doshiftvalue = true},
					bit_sqwEn = #bitParam{value = [?RTC_CTRL_BIT_SQWEN_EN, ?RTC_CTRL_BIT_SQWEN_DIS], mask = ?RTC_CTRL_BIT_SQWEN_MASK, doshiftvalue = true},
					bit_almA1 = #bitParam{value = [?RTC_CTRL_BIT_ALM_Ax_EN, ?RTC_CTRL_BIT_ALM_Ax_DIS], mask = ?RTC_CTRL_BIT_ALM_A1_MASK, doshiftvalue = true},
					bit_almA0 = #bitParam{value = [?RTC_CTRL_BIT_ALM_Ax_EN, ?RTC_CTRL_BIT_ALM_Ax_DIS], mask = ?RTC_CTRL_BIT_ALM_A0_MASK, doshiftvalue = true},
					bit_extOsc = #bitParam{value = [?RTC_CTRL_BIT_EXTOSC_EN, ?RTC_CTRL_BIT_EXTOSC_DIS], mask = ?RTC_CTRL_BIT_EXTOSC_MASK, doshiftvalue = true},
					bit_crsTrim = #bitParam{value = [?RTC_CTRL_BIT_CRSTRIM_EN, ?RTC_CTRL_BIT_CRSTRIM_DIS], mask = ?RTC_CTRL_BIT_CRSTRIM_MASK, doshiftvalue = true},
					bit_sqwfs = #bitParam{value = [?RTC_CTRL_BIT_SQWFS_1Hz, ?RTC_CTRL_BIT_SQWFS_4069Hz, ?RTC_CTRL_BIT_SQWFS_8192Hz, ?RTC_CTRL_BIT_SQWFS_32768Hz], mask = ?RTC_CTRL_BIT_SQWFS_MASK, doshiftvalue = true}
				   }).

%% ====================================================================
%% PWRxxMIN REGISTERs
%% ====================================================================
-define(RTC_PWR_DOWN_MIN_ADDR,	16#18).
-define(RTC_PWR_UP_MIN_ADDR,	16#1C).
-record(rtcPwrDNMinuteReg, {
					   address = ?RTC_PWR_DOWN_MIN_ADDR,
					   bit_minTen = #bitParam{value = ?RTC_MINUTE_BIT_MINTEN, mask = ?RTC_MINUTE_BIT_MINTEN_MASK, doshiftvalue = true},
					   bit_minOne = #bitParam{value = ?RTC_MINUTE_BIT_MINONE, mask = ?RTC_MINUTE_BIT_MINONE_MASK, doshiftvalue = true}
					  }).
-record(rtcPwrUPMinuteReg, {
					   address = ?RTC_PWR_UP_MIN_ADDR,
					   bit_minTen = #bitParam{value = ?RTC_MINUTE_BIT_MINTEN, mask = ?RTC_MINUTE_BIT_MINTEN_MASK, doshiftvalue = true},
					   bit_minOne = #bitParam{value = ?RTC_MINUTE_BIT_MINONE, mask = ?RTC_MINUTE_BIT_MINONE_MASK, doshiftvalue = true}
					  }).

%% ====================================================================
%% PWRxxHOUR REGISTERs
%% ====================================================================
-define(RTC_PWR_DOWN_HOUR_ADDR,	16#19).
-define(RTC_PWR_UP_HOUR_ADDR,	16#1D).
-record(rtcPwrDNHourReg, {
						 address = ?RTC_PWR_DOWN_HOUR_ADDR,
						 bit_timeFormat = #bitParam{value = [?RTC_HOUR_BIT_TIME_FORMAT_12H, ?RTC_HOUR_BIT_TIME_FORMAT_24H], mask = ?RTC_HOUR_BIT_TIME_FORMAT_MASK, doshiftvalue = true},
		
						 %% 12H mode
						 bit_12h_ampm = #bitParam{value = [?RTC_HOUR_BIT_12H_AMPM_IND_PM, ?RTC_HOUR_BIT_12H_AMPM_IND_AM], mask = ?RTC_HOUR_BIT_12H_AMPM_IND_MASK, doshiftvalue = true},
						 bit_12h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_12H_HRTEN0, mask = ?RTC_HOUR_BIT_12H_HRTEN0_MASK, doshiftvalue = true},
						 bit_12h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_12H_HRONE, mask = ?RTC_HOUR_BIT_12H_HRONE_MASK, doshiftvalue = true},
						 
						 %% 24H mode
						 bit_24h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_24H_HRTEN, mask = ?RTC_HOUR_BIT_24H_HRTEN_MASK, doshiftvalue = true},
						 bit_24h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_24H_HRONE, mask = ?RTC_HOUR_BIT_24H_HRONE_MASK, doshiftvalue = true}
				}).
-record(rtcPwrUPHourReg, {
						 address = ?RTC_PWR_UP_HOUR_ADDR,
						 bit_timeFormat = #bitParam{value = [?RTC_HOUR_BIT_TIME_FORMAT_12H, ?RTC_HOUR_BIT_TIME_FORMAT_24H], mask = ?RTC_HOUR_BIT_TIME_FORMAT_MASK, doshiftvalue = true},
		
						 %% 12H mode
						 bit_12h_ampm = #bitParam{value = [?RTC_HOUR_BIT_12H_AMPM_IND_PM, ?RTC_HOUR_BIT_12H_AMPM_IND_AM], mask = ?RTC_HOUR_BIT_12H_AMPM_IND_MASK, doshiftvalue = true},
						 bit_12h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_12H_HRTEN0, mask = ?RTC_HOUR_BIT_12H_HRTEN0_MASK, doshiftvalue = true},
						 bit_12h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_12H_HRONE, mask = ?RTC_HOUR_BIT_12H_HRONE_MASK, doshiftvalue = true},
						 
						 %% 24H mode
						 bit_24h_hrTen = #bitParam{value = ?RTC_HOUR_BIT_24H_HRTEN, mask = ?RTC_HOUR_BIT_24H_HRTEN_MASK, doshiftvalue = true},
						 bit_24h_hrOne = #bitParam{value = ?RTC_HOUR_BIT_24H_HRONE, mask = ?RTC_HOUR_BIT_24H_HRONE_MASK, doshiftvalue = true}
				}).

%% ====================================================================
%% PWRxxDATE REGISTERs
%% ====================================================================
-define(RTC_PWR_DOWN_DATE_ADDR,	16#1A).
-define(RTC_PWR_UP_DATE_ADDR,	16#1E).
-record(rtcPwrDNDateReg, {
						address = ?RTC_PWR_DOWN_DATE_ADDR,
						bit_dateTen = #bitParam{value = ?RTC_DATE_BIT_DATETEN, mask = ?RTC_DATE_BIT_DATETEN_MASK, doshiftvalue = true},
						bit_dateOne = #bitParam{value = ?RTC_DATE_BIT_DATEONE, mask = ?RTC_DATE_BIT_DATEONE_MASK, doshiftvalue = true}
					   }).
-record(rtcPwrUPDateReg, {
						address = ?RTC_PWR_UP_DATE_ADDR,
						bit_dateTen = #bitParam{value = ?RTC_DATE_BIT_DATETEN, mask = ?RTC_DATE_BIT_DATETEN_MASK, doshiftvalue = true},
						bit_dateOne = #bitParam{value = ?RTC_DATE_BIT_DATEONE, mask = ?RTC_DATE_BIT_DATEONE_MASK, doshiftvalue = true}
					   }).

%% ====================================================================
%% PWRxxMONTH REGISTERs
%% ====================================================================
-define(RTC_PWR_DOWN_MONTH_ADDR,16#1B).
-define(RTC_PWR_UP_MONTH_ADDR,	16#1F).
-define(RTC_PWR_MONTH_BIT_WKDAY,		{1,7}).
-define(RTC_PWR_MONTH_BIT_WKDAY_MASK,	2#11100000).

-define(RTC_PWR_MONTH_BIT_MTHTEN0,		{0,1}).
-define(RTC_PWR_MONTH_BIT_MTHTEN0_MASK,	2#00010000).

-define(RTC_PWR_MONTH_BIT_MTHONE,		{0,9}).
-define(RTC_PWR_MONTH_BIT_MTHTONE_MASK,	2#00001111).

-record(rtcPwrDNMonthReg, {
						 address = ?RTC_PWR_DOWN_MONTH_ADDR,
						 bit_wDay = #bitParam{value = ?RTC_PWR_MONTH_BIT_WKDAY, mask = ?RTC_PWR_MONTH_BIT_WKDAY_MASK, doshiftvalue = true},
						 bit_monthTen = #bitParam{value = ?RTC_PWR_MONTH_BIT_MTHTEN0, mask = ?RTC_PWR_MONTH_BIT_MTHTEN0_MASK, doshiftvalue = true},
						 bit_monthOne = #bitParam{value = ?RTC_PWR_MONTH_BIT_MTHONE, mask = ?RTC_PWR_MONTH_BIT_MTHTONE_MASK, doshiftvalue = true}
						}).

-record(rtcPwrUPMonthReg, {
						 address = ?RTC_PWR_UP_MONTH_ADDR,
						 bit_wDay = #bitParam{value = ?RTC_PWR_MONTH_BIT_WKDAY, mask = ?RTC_PWR_MONTH_BIT_WKDAY_MASK, doshiftvalue = true},
						 bit_monthTen = #bitParam{value = ?RTC_PWR_MONTH_BIT_MTHTEN0, mask = ?RTC_PWR_MONTH_BIT_MTHTEN0_MASK, doshiftvalue = true},
						 bit_monthOne = #bitParam{value = ?RTC_PWR_MONTH_BIT_MTHONE, mask = ?RTC_PWR_MONTH_BIT_MTHTONE_MASK, doshiftvalue = true}
						}).

%% ====================================================================
%% SRAM REGISTERs
%% ====================================================================
-define(RTC_SRAM_ADDR_RANGE,	{16#20, 16#5F}).

%% ====================================================================
%% Type definitions
%% ====================================================================
-type data()			::	0..255.
-type address()			::	integer().
-type bitfield_value()	::	0..255.	%% The end bitfield_value is not really 255, but what is supported by the OS and CPU.
-type bitfield_mask()	::	0..255.	%% The end bitfield_value is not really 255, but what is supported by the OS and CPU.
-type register_rec()	::	tuple().%% The tuple version of any regoster record. eq: #rtcControlReg{}

-type hour_am()			::	?RTC_HOUR_BIT_12H_AMPM_IND_AM.
-type hour_pm()			::	?RTC_HOUR_BIT_12H_AMPM_IND_PM.
-type hour_ampm_ind()	::	hour_am() | hour_pm().
-type second()			::	0..59.
-type minute()			::	0..59.
-type hour()			::	0..23.
-type hour12()			::	0..12.
-type day()				::	1..31.
-type wday()			::	1..7.
-type month()			::	1..12.
-type year()			::	integer().
-type time()			::	{hour(), minute(), second()} | {{hour_ampm_ind(), hour12()}, minute(), second()}.
-type date()			::	{year(), month(), day()}.
-type datetime()		::	{date(), time()}.

-type time_format_12h()	::	?RTC_HOUR_BIT_TIME_FORMAT_12H.
-type time_format_24h()	::	?RTC_HOUR_BIT_TIME_FORMAT_24H.
-type time_format()		::	time_format_12h() | time_format_24h().

-type rtc_alarm_id()	::	?RTC_ALARM_0_ID..?RTC_ALARM_1_ID.

-type rtc_alarm_mask_sec()			::	?RTC_ALMxWKDAY_BIT_ALMxMASK_SEC_MATCH.
-type rtc_alarm_mask_min()			::	?RTC_ALMxWKDAY_BIT_ALMxMASK_MIN_MATCH.
-type rtc_alarm_mask_hour()			::	?RTC_ALMxWKDAY_BIT_ALMxMASK_HOUR_MATCH.
-type rtc_alarm_mask_wday()			::	?RTC_ALMxWKDAY_BIT_ALMxMASK_WDAY_MATCH.
-type rtc_alarm_mask_all()			::	?RTC_ALMxWKDAY_BIT_ALMxMASK_ALL_MATCH.
-type rtc_alarm_mask()				::	rtc_alarm_mask_sec() | rtc_alarm_mask_min() | rtc_alarm_mask_hour() | rtc_alarm_mask_wday() | rtc_alarm_mask_all().

-type rtc_alarm_interrupt_set()		::	?RTC_ALMxWKDAY_BIT_ALMxIF_SET.
-type rtc_alarm_interrupt_clear()	::	?RTC_ALMxWKDAY_BIT_ALMxIF_CLEAR.

-type rtc_alarm_interrupt_en()			::	?RTC_CTRL_BIT_ALM_Ax_EN.
-type rtc_alarm_interrupt_dis()			::	?RTC_CTRL_BIT_ALM_Ax_DIS.
-type rtc_alarm_interrupt_en_status()	::	rtc_alarm_interrupt_en() | rtc_alarm_interrupt_dis().

-type rtc_alarm_interrupt_out_pol_high()::	?RTC_ALMxWKDAY_BIT_ALMPOL_HIGH.
-type rtc_alarm_interrupt_out_pol_low()	::	?RTC_ALMxWKDAY_BIT_ALMPOL_LOW.
-type rtc_alarm_interrupt_out_pol()		::	rtc_alarm_interrupt_out_pol_high() | rtc_alarm_interrupt_out_pol_low().

-type rtc_ctrl_bit_out_high()	::	?RTC_CTRL_BIT_OUT_MFP_HIGH.
-type rtc_ctrl_bit_out_low()	::	?RTC_CTRL_BIT_OUT_MFP_LOW.
-type rtc_ctrl_bit_out()		::	rtc_ctrl_bit_out_high() | rtc_ctrl_bit_out_low().

-type rtc_ctrl_bit_sqwen_en()	::	?RTC_CTRL_BIT_SQWEN_EN.
-type rtc_ctrl_bit_sqwen_dis()	::	?RTC_CTRL_BIT_SQWEN_DIS.
-type rtc_ctrl_bit_sqwen()		::	rtc_ctrl_bit_sqwen_en() | rtc_ctrl_bit_sqwen_dis().

-type rtc_ctrl_bit_extosc_en()	::	?RTC_CTRL_BIT_EXTOSC_EN.
-type rtc_ctrl_bit_extosc_dis()	::	?RTC_CTRL_BIT_EXTOSC_DIS.
-type rtc_ctrl_bit_extosc()		::	rtc_ctrl_bit_extosc_en() | rtc_ctrl_bit_extosc_dis().

-type rtc_ctrl_bit_crstrim_en()	::	?RTC_CTRL_BIT_CRSTRIM_EN.
-type rtc_ctrl_bit_crstrim_dis()::	?RTC_CTRL_BIT_CRSTRIM_DIS.
-type rtc_ctrl_bit_crstrim()	::	rtc_ctrl_bit_crstrim_en() | rtc_ctrl_bit_crstrim_dis().

-type rtc_ctrl_bit_sqwfs_1Hz()		::	?RTC_CTRL_BIT_SQWFS_1Hz.
-type rtc_ctrl_bit_sqwfs_4096Hz()	::	?RTC_CTRL_BIT_SQWFS_4069Hz.
-type rtc_ctrl_bit_sqwfs_8192Hz()	::	?RTC_CTRL_BIT_SQWFS_8192Hz.
-type rtc_ctrl_bit_sqwfs_32768Hz()	::	?RTC_CTRL_BIT_SQWFS_32768Hz.
-type rtc_ctrl_bit_sqwfs()			::	rtc_ctrl_bit_sqwfs_1Hz() | rtc_ctrl_bit_sqwfs_4096Hz() | rtc_ctrl_bit_sqwfs_8192Hz() | rtc_ctrl_bit_sqwfs_32768Hz().

-type rtc_pwrfail_primary_power_lost()		::	?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_LOST.
-type rtc_pwrfail_primary_power_not_lost()	::	?RTC_WKDAY_BIT_PWRFAIL_PRIM_PWR_NOT_LOST.
-type rtc_pwrfail()							::	rtc_pwrfail_primary_power_lost() | rtc_pwrfail_primary_power_not_lost().

-type rtc_vbaten_en()	::	?RTC_WKDAY_BIT_VBATEN_EN.
-type rtc_vbaten_dis()	::	?RTC_WKDAY_BIT_VBATEN_DIS.
-type rtc_vbaten()		::	rtc_vbaten_en() | rtc_vbaten_dis().

-type rtc_oscrun_en()	::	?RTC_WKDAY_BIT_OSCRUN_EN.
-type rtc_oscrun_dis()	::	?RTC_WKDAY_BIT_OSCRUN_DIS.
-type rtc_oscrun()		::	rtc_oscrun_en() | rtc_oscrun_dis().

-define(RTC_PWR_REGISTER_TYPE_DOWN,	down).
-define(RTC_PWR_REGISTER_TYPE_UP,	up).
-type rtc_pwr_register_down()	::	?RTC_PWR_REGISTER_TYPE_DOWN.
-type rtc_pwr_register_up()		::	?RTC_PWR_REGISTER_TYPE_UP.
-type rtc_pwr_register()		::	rtc_pwr_register_down() | rtc_pwr_register_up().

-endif.
