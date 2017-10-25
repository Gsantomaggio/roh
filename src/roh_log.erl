%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2017 15:12
%%%-------------------------------------------------------------------
-module(roh_log).
-author("gabriele").

%% API
-export([]).

-callback info(Msg::string(),Params::list()) -> ok.
-callback debug(Msg::string(),Params::list()) -> ok.
-callback warning(Msg::string(),Params::list()) -> ok.
-callback error(Msg::string(),Params::list()) -> ok.
-callback out(Msg::atom(),Params::list()) -> ok.
-callback level(Msg::atom()) -> ok.


