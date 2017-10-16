%%%-------------------------------------------------------------------
%% @doc roh top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(roh_sup).

-behaviour(supervisor).

-include_lib("../include/roh_headers.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = {one_for_one, 10, 60},
    ChildSpec = [{roh_pool, {roh_pool, start_link, [roh_publish_worker]},
        permanent, brutal_kill, worker, [roh_pool]}],

    {ok, {RestartStrategy, ChildSpec}}.
