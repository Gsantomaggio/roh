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

-behaviour(supervisor).


-define(SERVER, ?MODULE).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?SERVER, []).

init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.