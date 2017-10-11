%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2017 12:08
%%%-------------------------------------------------------------------
-module(roh_worker_sup).
-author("gabriele").
-include("../include/roh_headers.hrl").


-behaviour(supervisor).


-define(SERVER, ?MODULE).
-define(SHUTDOWN_TIMEOUT, 5000).
-define(WORKER(I), {I, {I, start_link, []}, temporary, ?SHUTDOWN_TIMEOUT, worker, [I]}).

-export([start_link/1, init/1]).

start_link(WorkerModule) ->
    supervisor:start_link(?MODULE, [WorkerModule]).

init([WorkerModule]) ->
    Child = ?WORKER(WorkerModule),
    {ok, {{simple_one_for_one, 0, 1}, [Child]}}.



