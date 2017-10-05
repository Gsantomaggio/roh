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

-record(state, {supervisor, proc_opened = 0, length = 0, tasks = maps:new(), global = 0}).

%%%===================================================================
%%% API
%%%===================================================================


stress(N) when N > 0 ->
    add_task(),
    stress(N - 1);
stress(_N) -> ok.


add_task() ->
    UUID = erlang:phash2({rand:uniform(500), now()}),
    Task = #task{id = UUID, body = "{\"name\":\"test_json\"}", time_start = now()},
    R = gen_server:call(?SERVER, {add_task, Task}),
    R.
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
    {ok, PID} = roh_worker_sup:start_link(),
    {ok, #state{supervisor = PID}}.

maybe_run_next(M, Sup) ->
    case maps:size(M) of
        0 -> ok;
        N when N > 0 -> First = maps:get(lists:nth(1, maps:keys(M)), M),
            new_worker(First, Sup, self())

    end,

    ok.

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
handle_call({add_task, Task = #task{id = UUID}}, _From, State = #state{proc_opened = PO, tasks = M, supervisor = Sup}) when PO =< 5 ->
    M2 = append_task(Task, M),
    A = new_worker(Task, Sup, self()),
    roh_console_log:info("Child started, id: ~w, Task body: ~s PID: ~w ~n", [UUID, Task#task.body, A]),
    {reply, ok, State#state{tasks = M2, proc_opened = State#state.proc_opened + 1, length = maps:size(M2), global = State#state.global + 1}};
handle_call({add_task, Task}, _From, State = #state{tasks = M}) ->
    M2 = append_task(Task, M),
    roh_console_log:info("Added, waiting list: ~w ~n:", [maps:size(M2)]),
    {reply, ok, State#state{tasks = M2, length = maps:size(M2), global = State#state.global + 1}};
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
handle_cast({remove_task, ID}, State = #state{tasks = M, supervisor = Sup, global = G}) ->
    MD = maps:remove(ID, M),
    stop_worker(ID, Sup),
    roh_console_log:info("Removing task ~w len: ~w Total:~w ~n", [ID, maps:size(MD), G]),
    maybe_run_next(MD, Sup),
    {noreply, State#state{tasks = MD, proc_opened = State#state.proc_opened - 1}};
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


append_task(Task, Map) ->
    maps:put(Task#task.id, Task, Map).


new_worker(Task, SupervisorPID, ServerPID) ->
    C = {Task#task.id, {roh_worker, start_link, [{Task, ServerPID}]},
        transient, 5000, worker, [roh_worker]},
    supervisor:start_child(SupervisorPID, C).

stop_worker(ID, ServerPID) ->
    supervisor:terminate_child(ServerPID, ID),
    supervisor:delete_child(ServerPID, ID).
