%%% @author Ivan Iacono <ivan.iacono@erlang-solutions.com> - Erlang Solutions Ltd
%%% @copyright (C) 2013, Erlang Solutions Ltd
%%% @doc This module allow to use erlang/ALE to generate PWM signals.
%%% @end

-module(pwm).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, value/1]).
-export([load_nif/0, init/0, pwm_value/1, pwm_release/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-onload([load_nif/0]).

-define(SERVER, ?MODULE).
-define(NIF_PWM_LIB, "../priv/pwm_nif").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

value(X) -> 
    gen_server:call(?SERVER, {call, value, X}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    load_nif(),
    init(),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({call, value, X}, _From, State) ->
    Reply = pwm_value(X),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    pwm_release(),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Load the PWM C library.
%% @end
-spec(load_nif() -> ok | {error, error_type}).

load_nif() ->
    ok = erlang:load_nif(?NIF_PWM_LIB, 0).

%% @doc Initialise the PWM peripheral.
%% @end
-spec(init() -> ok | {error,pwm_initialization_error}).
init() ->
    exit({error, pwm_initialization_error}).

%% @doc Set PWM value.
%% @end
-type val() :: 0..1024.
-spec(pwm_value(val()) -> ok).
pwm_value(_Val) ->
    {error, pwm_value_error}.

%% @doc Releases the PWM peripheral and unmaps the memory.
%% @end
-spec(pwm_release() -> ok).
pwm_release() ->
    exit({error, pwm_release_error}).
