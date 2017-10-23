%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Oct 2017 17:36
%%%-------------------------------------------------------------------
-module(roh_pool).
-author("gabriele").

-behaviour(gen_server).


-include_lib("../include/roh_headers.hrl").
%% API
-export([start_link/1, add_task/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    stop_tasks/0]).

-define(SERVER, ?MODULE).
-define(MAX_TASKS, 20).


-record(state, {
    supervisor,
    running_workers = maps:new(),
    waiting_queue = queue:new(),
    worker_module,
    global = 0}).

%%%===================================================================
%%% API
%%%===================================================================


add_task(Task) ->
    gen_server:call(?SERVER, {add_task, Task}).

stop_tasks() ->
    gen_server:call(?SERVER, {stop_all_tasks}).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(WorkerModule :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(WorkerModule) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [WorkerModule], []).

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
init([WorkerModule]) ->
    process_flag(trap_exit, true),
    {ok, PID} = roh_worker_sup:start_link(WorkerModule),
    {ok, #state{supervisor = PID, worker_module = WorkerModule}}.


is_watermark_processes(MRW) ->
    case maps:size(MRW) of
        Value when Value < ?MAX_TASKS -> false;
        Value when Value >= ?MAX_TASKS -> true
    end.



maybe_run_next_Q(QWQ, Sup, MRW) ->
    case is_watermark_processes(MRW) of
        true -> {MRW, QWQ};
        false -> case queue:out(QWQ) of
                     {empty, QWQ1} -> {MRW, QWQ1};
                     {{value, Task}, QWQ2} ->
                         {execute_new_worker(Task, Sup, MRW), QWQ2}
                 end
    end.


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
handle_call({add_task, Task}, _From,
    State = #state{running_workers = MRW, waiting_queue = QWQ, supervisor = Sup}) ->

    case is_watermark_processes(MRW) of
        true ->
            QWQ2 = queue:in(Task, QWQ),
            roh_console_log:info("Added in waiting list, current size: ~w", [queue:len(QWQ2)]),
            {reply, ok, State#state{waiting_queue = QWQ2, global = State#state.global + 1}};
        false -> MRW2 = execute_new_worker(Task, Sup, MRW),
            {reply, ok, State#state{running_workers = MRW2, global = State#state.global + 1}}
    end;

handle_call({stop_all_tasks}, _From,
    State = #state{running_workers = MRW, waiting_queue = QWQ, supervisor = Sup}) ->
    L = maps:to_list(MRW),
    [gen_server:cast(K, {stop}) || {K, _} <- L],
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast(_Request, State) ->
    {noreply, State}.

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
handle_info({'EXIT', WorkerPid, Reason}, State = #state{running_workers = MRW, waiting_queue = QWQ, supervisor = Sup, global = G}) ->
    MRW2 = maps:remove(WorkerPid, MRW),
    {MRW3, QWQ2} = maybe_run_next_Q(QWQ, Sup, MRW2),
    stop_worker(WorkerPid, Sup),
    roh_console_log:info("EXIT: Removing task, Waiting queue: ~w  Child PID: ~w Runnig Workers: ~w Total:~w Reason: ~w, erlang proccesses ~w", [queue:len(QWQ2), WorkerPid, maps:size(MRW3), G, Reason, length(processes())]),
    {noreply, State#state{running_workers = MRW3, waiting_queue = QWQ2}};
handle_info(_Request, State) ->
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



execute_new_worker(Task, Sup, MRW) ->
    WorkerPID = new_worker(Sup),
    M = move_worker_to_runners(MRW, WorkerPID, Task),
    send_async_task(WorkerPID, Task, Sup),
    M.


move_worker_to_runners(MRW, WorkerPID, Task) ->
    maps:put(WorkerPID, Task, MRW).

send_async_task(WorkerPID, Task, Sup) ->
    gen_server:cast(WorkerPID, {start, Task, Sup}).


new_worker(Sup) ->
    {ok, WorkerPID} = supervisor:start_child(Sup, []),
    link(WorkerPID),
    WorkerPID.


stop_worker(ID, ServerPID) ->
    supervisor:terminate_child(ServerPID, ID),
    unlink(ID),
    supervisor:delete_child(ServerPID, ID).
