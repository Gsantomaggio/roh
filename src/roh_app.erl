%%%-------------------------------------------------------------------
%% @doc roh public API
%% @end
%%%-------------------------------------------------------------------

-module(roh_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    start(normal, normal).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/management", rest_management, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    roh_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
