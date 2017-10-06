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


-include("../include/roh_headers.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    add_task/0,
    stress/1]).

-define(SERVER, ?MODULE).
-define(MAX_PROCESSES, 3).


-record(state, {
    supervisor,
    running_workers = maps:new(),
    waiting_queue = queue:new(),
    global = 0}).

%%%===================================================================
%%% API
%%%===================================================================


stress(N) when N > 0 ->
    add_task(),
%%    timer:sleep(200),
    stress(N - 1);
stress(_N) -> ok.


add_task() ->
    UUID = erlang:phash2({rand:uniform(500), now()}),
    Task = #task{id = UUID, body = "{\"name\":\"test_json\"}", time_start = now()},
    R = add_task(Task),
    R.

add_task(Task) ->
    gen_server:call(?SERVER, {add_task, Task}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    process_flag(trap_exit, true),
    {ok, PID} = roh_worker_sup:start_link(),
    {ok, #state{supervisor = PID}}.


is_the_limit_reached(MRW) ->
    case maps:size(MRW) of
        Value when Value < ?MAX_PROCESSES -> false;
        Value when Value >= ?MAX_PROCESSES -> true
    end.



maybe_run_next_Q(QWQ, Sup, MRW) ->
    case is_the_limit_reached(MRW) of
        false -> case queue:out(QWQ) of
                     {empty, QWQ1} -> {MRW, QWQ1};
                     {{value, Task}, QWQ2} ->
                         {add_new_worker_task(Task, Sup, MRW), QWQ2}
                 end;
        true -> {MRW, QWQ}
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

    case is_the_limit_reached(MRW) of
        false -> MRW2 = add_new_worker_task(Task, Sup, MRW),
            {reply, ok, State#state{running_workers = MRW2, global = State#state.global + 1}};
        true ->
            QWQ2 = queue:in(Task, QWQ),
            roh_console_log:info("Added in waiting list, current size: ~w", [queue:len(QWQ2)]),
            {reply, ok, State#state{waiting_queue = QWQ2, global = State#state.global + 1}}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

add_new_worker_task(Task, Sup, MRW) ->
    {ok, WorkerPID} = new_worker(Task, Sup, self()),
    roh_console_log:info("Child started, Supervisor id: ~w, Task body: ~s WorkerPID: ~w ", [Sup, Task#task.body, WorkerPID]),
    link(WorkerPID),
    maps:put(WorkerPID, Task, MRW).

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




new_worker(Task, SupervisorPID, ServerPID) ->
    supervisor:start_child(SupervisorPID, [{Task, ServerPID}]).

stop_worker(ID, ServerPID) ->
    supervisor:terminate_child(ServerPID, ID),
    unlink(ID),
    supervisor:delete_child(ServerPID, ID).
