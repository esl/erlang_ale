%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This header file contains common definitions for ALE.
%% @end

-ifndef(ALE_COMMON_HRL).
-define(ALE_COMMON_HRL,true).

%% ====================================================================
%% I2C and SPI device name definitions.
%% ====================================================================
-define(I2C_DEVICE_CH1_NAME, "i2c-1").
-define(SPI_DEVICE_CH0_NAME, "spidev0.0").
-define(SPI_DEVICE_DEFAULT_OPTIONS, []).

%% ====================================================================
%% Driver module and its start, stop function definitions.
%% ====================================================================
-define(DRV_GPIO_MODULE, gpio).
-define(DRV_I2C_MODULE, i2c).
-define(DRV_SPI_MODULE, spi).

-define(START_FUNC_DRV_MODULE, start_link).
-define(STOP_FUNC_DRV_MODULE, stop).

%% ====================================================================
%% Define for common ERROR_INFO and ERROR_REPORT structure.
%% ====================================================================
-define(DO_ERR(TEXT,TUPPLELIST), error_logger:error_report(lists:append([TEXT], lists:append(TUPPLELIST,[{module, ?MODULE}, {line, ?LINE}])))).
-define(DO_INFO(TEXT,TUPPLELIST), error_logger:info_report(lists:append([TEXT], lists:append(TUPPLELIST,[{module, ?MODULE}, {line, ?LINE}])))).
-define(DO_WAR(TEXT,TUPPLELIST), error_logger:info_warning(lists:append([TEXT], lists:append(TUPPLELIST,[{module, ?MODULE}, {line, ?LINE}])))).

-endif.