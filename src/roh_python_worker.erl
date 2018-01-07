%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Oct 2017 09:43
%%%-------------------------------------------------------------------
-module(roh_python_worker).
-author("gabriele").
-include_lib("../include/roh_headers.hrl").

-behaviour(roh_worker).


%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {python_instance = no_istance, current_task = no_task}).


get_python_path() ->

    case application:get_env(roh, python_scripts_path) of
        undefined -> {ok, P} = file:get_cwd(),
            P;
        Value -> {ok, P} = Value,
            P
    end.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    roh_console_log:info("The current directory is: ~s", [get_python_path()]),
    {ok, PythonInstance} = python:start([{python_path, get_python_path() ++ "/python_scripts/"}, {python, "python"}]),
    {ok, #state{python_instance = PythonInstance}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({start, Task = #task{id = UUID, module_start = M, function_start = F, parameters_start = Prs}, _Sup}, _From, State = #state{python_instance = PythonInstance}) ->
    roh_console_log:info("Sync Script worker ~w, function ~s, parameters ~w", [UUID, F, Prs]),
    python:call(PythonInstance, M, F, Prs),
    roh_console_log:info("Sync end Script worker ~w, function ~s, parameters ~w", [UUID, F, Prs]),
    {reply, ok, State#state{current_task = Task}, hibernate};
handle_call({stop}, _From, State = #state{python_instance = PythonInstance, current_task = CT}) ->
    roh_console_log:info("Sync Stopping... ~w", [CT#task.id]),
    python:call(PythonInstance, CT#task.module_stop, CT#task.function_stop, CT#task.parameters_stop),
    python:stop(PythonInstance),
    {stop, normal, State#state{python_instance = no_instance, current_task = no_task}, hibernate}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({start, Task = #task{id = UUID, module_start = M, function_start = F, parameters_start = Prs}, _Sender}, State = #state{python_instance = PythonInstance}) ->
    roh_console_log:info("Script worker ~w, function ~s, parameters ~w", [UUID, F, Prs]),
    python:call(PythonInstance, M, F, Prs),
    roh_console_log:info("Script END worker ~w, function ~s, parameters ~w", [UUID, F, Prs]),
    {noreply, State#state{current_task = Task}};
handle_cast({stop}, State = #state{python_instance = PythonInstance, current_task = CT}) ->
    roh_console_log:info("Stopping... ~w", [CT#task.id]),
    python:call(PythonInstance, CT#task.module_stop, CT#task.function_stop, CT#task.parameters_stop),
    python:stop(PythonInstance),
    {stop, normal, State#state{python_instance = no_instance, current_task = no_task}}.


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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, #state{python_instance = P}) when P =/= no_istance andalso is_pid(P) ->
    Stop_Result = python:stop(P),
    roh_console_log:info("Closed ~w, Stop Result ~s", [Reason, Stop_Result]),
    ok;
terminate(Reason, _State) ->
    roh_console_log:info("Stopped normally ~w", [Reason]),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
