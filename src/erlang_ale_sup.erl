%% @author ethrbh, Robert Balogh, ethrbh@gmail.com
%% @copyright (C) 2015, Robert Balogh
%% @doc
%% This is the supervisor module for Erlang/ALE.
%% @end

-module(erlang_ale_sup).
-behaviour(supervisor).

-export([start_driver_process/4, stop_driver_process/1]).
-export([start_link/0]).
-export([init/1]).

%%===================================================================
%% Includes
%%===================================================================
-include("ale_type_def.hrl").
-include("ale_common.hrl").

%%===================================================================
%% Defines
%%===================================================================
-define(SERVER, ?MODULE).

-record(rALEHandler, {
					  key,			%% tuple, {gpio, pin(), pin_direction()} | {i2c, devname(), addr()} | {spi, list()}
					  drvPid,		%% pid(), pid of the driver process
					  initialMFA,	%% mfa()
					  childId		%% term(), ID of child within supervisor
					  }).

-define(ALE_HANDLER_TABLE, ale_handler_table_new).
-define(ALE_HANDLER_TABLE_KEYPOS, 2).

%%===================================================================
%% @doc
%% Start the supervisor.
%% @end
-spec start_link() -> {ok, pid()} | {already_started, pid()} | {shutdown, term()} | {error, term()} | ignore | term().
%%===================================================================
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ====================================================================
%% @doc
%% Start driver process if not started yet.
%% Input:
%%		InitialMFA			: tuple, the MFA of initial setup of driver process.
%%		DrvModule			: atom
%%		DrvStartFunction	: atom
%%		Arg					: list, [term()]
%% Output:
%%		{ok,pid()} | {error, term()}
%% @end
-spec start_driver_process(mfa(), atom(), atom(), list(term())) -> {ok,pid()} | {error, term()}.
%% ====================================================================
start_driver_process(InitialMFA, DrvModule, DrvStartFunction, Arg) ->
	
	%% Start supervisor if that is not running.
	SuperVisorRes = case whereis(?SERVER) of
						P when is_pid(P) ->
							{ok,P};
						_-> %% Supervisor not started yet. Do it.
							start_link()
					end,
	case SuperVisorRes of
		{ok, _SuperVisorPid} ->
			%% Start driver process if not started yet.
			case get_driver_process(DrvModule, DrvStartFunction, Arg) of
				{ok,R} ->
					%% Driver process already started. No need to do anything.
					{ok, R#rALEHandler.drvPid};
				
				_->	%% Driver process does not started yet. Do it.
					ChildId = InitialMFA,
		            Res = case catch supervisor:start_child(?SERVER,
															{ChildId, 
															 {DrvModule, DrvStartFunction, Arg},
															 transient, 10, worker, [DrvModule]}) of
							  {'EXIT',R} ->
								  {error, {'EXIT',R}};
							  
							  {ok, DrvPid} when is_pid(DrvPid) ->
								  case DrvModule of
									  ?DRV_GPIO_MODULE ->
										  [Gpio, PinDirection] = Arg,
										  
										  %% Handle special case for interrupt handling on Gpio.
										  SpecialCaseRes = case InitialMFA of
															   {_Module, gpio_set_int, [_Gpio, _IntCondition, Destination]} ->
																   %% Register interrupt process
																   case erlang:apply(?DRV_GPIO_MODULE, register_int, [DrvPid, Destination]) of
																	   ok ->
																		   ok;
																	   {error, R} ->
																		   {error, R}
																   end;
															   _-> ok
														   end,
										  case SpecialCaseRes of
											  ok ->
												  {ok, {?DRV_GPIO_MODULE, Gpio, PinDirection}, DrvPid};
											  ER->ER
										  end;
									  
									  ?DRV_I2C_MODULE ->
										  [DeviceName, HWAddress] = Arg,
										  {ok, {?DRV_I2C_MODULE, DeviceName, HWAddress}, DrvPid};
									  
									  ?DRV_SPI_MODULE ->
										  [DeviceName, SpiOptions] = Arg,
										  {ok, {?DRV_SPI_MODULE, DeviceName, SpiOptions}, DrvPid};
									  
									  _-> %% Unsupported scenario.
										  %% FIXME
										  {error, {unsupported_drv_module, DrvModule}}
								  end;
							  ER->ER
						  end,
					
					case Res of
						{ok, Key, DrvPidT} ->
							%% Save all in ETS.
							ets:insert(?ALE_HANDLER_TABLE, #rALEHandler{key = Key,
																		drvPid = DrvPidT,
																		initialMFA = InitialMFA,
																		childId = ChildId}),
							
							?DO_INFO("ALE driver process has been started and registered successfully.",
									 [{childId, ChildId},
									  {initialMFA, InitialMFA},
									  {drvModule, DrvModule},
									  {drvStartFunction, DrvStartFunction},
									  {drvStartArgs, Arg},
									  {drvPid, DrvPidT}]),
							
							{ok, DrvPidT};
						Error->
							?DO_ERR("Error occured when start ALE driver process.",
									[{childId, ChildId},
									 {initialMFA, InitialMFA},
									 {drvModule, DrvModule},
									 {drvStartFunction, DrvStartFunction},
									 {drvStartArgs, Arg},
									 {reason, Error}]),
							Error
					end
			end;
		ER->
			ER
	end.

%% ====================================================================
%% @doc
%% Stop driver process.
%% Input:
%%		DrvPid	:	pid()
%% Output:
%%		ok | {error, term()}
%% @end
-spec stop_driver_process(pid()) -> ok | {error, term()}.
%% ====================================================================
stop_driver_process(DrvPid) ->
	%% Start supervisor if that is not running.
	SuperVisorRes = case whereis(?SERVER) of
						P when is_pid(P) ->
							{ok,P};
						_-> %% Supervisor not started yet. Do it.
							start_link()
					end,
	case SuperVisorRes of
		{ok, _SuperVisorPid} ->
			case erlang:is_process_alive(DrvPid) of
				true ->
					%% Process is alive.
					case ets:match_object(?ALE_HANDLER_TABLE, #rALEHandler{drvPid = DrvPid, _='_'}) of
						[R] when is_record(R, rALEHandler) ->
							SpecialCaseRes = case erlang:element(1, R#rALEHandler.key) of
												 ?DRV_GPIO_MODULE ->
													 case R#rALEHandler.initialMFA of
														 {?MODULE, gpio_set_int, [_Gpio, _IntCondition, Destination]} ->
															 %% Unregister interrupt process
															 case erlang:apply(?DRV_GPIO_MODULE, unregister_int, [R#rALEHandler.drvPid, Destination]) of
																 ok ->
																	 {ok, ?DRV_GPIO_MODULE};
																 {error, R} ->
																	 {error, R}
															 end;
														 _-> %% Normal IO GPIO, so no interrupt has been configured.
															 {ok, ?DRV_GPIO_MODULE}
													 end;
												 
												 ?DRV_I2C_MODULE ->
													 {ok, ?DRV_I2C_MODULE};
												 
												 ?DRV_SPI_MODULE ->
													 {ok, ?DRV_SPI_MODULE};
												 
												 DrvModuleT ->
													 {error, {unsupported_drv_module, DrvModuleT}}
											 end,
							case SpecialCaseRes of
								{ok, DrvModule} ->
									%% Terminate child process
									supervisor:terminate_child(?SERVER, R#rALEHandler.childId),
									
									%% Delete child process
									supervisor:delete_child(?SERVER, R#rALEHandler.childId),
									
									%% Delete record in ETS
									ets:delete(?ALE_HANDLER_TABLE, R#rALEHandler.key),
									
									?DO_INFO("ALE driver has been released.",
											 [{childId, R#rALEHandler.childId},
											  {drvPid, DrvPid},
											  {drvModule, DrvModule},
											  {record_in_ets, R}]),
									ok;
										
								ER->
									?DO_ERR("Failed to release ALE driver.",
											 [{childId, R#rALEHandler.childId},
											  {drvPid, DrvPid},
											  {record_in_ets, R},
											  {reason, ER}]),
									ER
							end;
						
						[] ->
							?DO_WAR("ALE driver does not started.",
									[{drvPid, DrvPid}]),
							ok;
						
						Recs when is_list(Recs) ->
							%% Upps, this is a major error, because driver was started multiple times with the same attributes,
							%% what is not allowed.
							{error, {driver_was_started_multiple_times, {DrvPid, Recs}}}
					end;
					
				false ->
					%% Process does not alive.
					{error, {process_is_not_alive, DrvPid}}
			end;
		ER->ER
	end.

%%===================================================================
%% @doc
%% Init the supervisor.
%% @end
%%===================================================================
init([]) ->
	ets:new(?ALE_HANDLER_TABLE, [ordered_set,public,named_table,{keypos,?ALE_HANDLER_TABLE_KEYPOS}]),

	Procs = [],
	{ok, {{one_for_one, 1, 5}, Procs}}.


%%===================================================================
%% Internal functions.
%%===================================================================

%% ====================================================================
%% Find driver process
%% Input:
%%		DrvModule			:	atom
%%		DrvStartFunction	:	atom
%%		Arg					:	list, [term()]
%% Output:
%%		{ok,rALEHandler{}} | {error, term()}
%% ====================================================================
get_driver_process(DrvModule, _DrvStartFunction, Arg) ->
	Key = case DrvModule of
			  ?DRV_GPIO_MODULE ->
				  [Gpio, PinDirection] = Arg,
				  {?DRV_GPIO_MODULE, Gpio, PinDirection};
			  
			  ?DRV_I2C_MODULE ->
				  [DeviceName, HWAddress] = Arg,
				  {?DRV_I2C_MODULE, DeviceName, HWAddress};
			  
			  ?DRV_SPI_MODULE ->
				  [DeviceName, SpiOptions] = Arg,
				  {?DRV_SPI_MODULE, DeviceName, SpiOptions};
			  
			  _-> %% Unsupported scenario.
				  %% FIXME
				  {error, {unsupported_drv_module, DrvModule}}
		  end,
	case Key of
		{error, R} ->
			{error, R};
		_->	case ets:lookup(?ALE_HANDLER_TABLE, Key) of
				[R] when is_record(R, rALEHandler) ->
					{ok, R};
				ER->{error, ER}
			end
	end.

get_driver_process(gpio, Gpio) ->
	%% Search for driver process by its gpio.
	case ets:match_object(?ALE_HANDLER_TABLE, #rALEHandler{key = {'_', Gpio, '_'}, _='_'}) of
		[R] when is_record(R, rALEHandler) ->
			{ok, R};
		Recs when is_list(Recs) ->
			{error, {driver_was_started_multiple_times, {gpio, {Gpio, Recs}}}};
		ER ->
			{error, ER}
	end.

