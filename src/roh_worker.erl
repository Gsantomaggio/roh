%%%-------------------------------------------------------------------
%%% @author gabriele
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Oct 2017 16:30
%%%-------------------------------------------------------------------
-module(roh_worker).
-author("gabriele").
%% API

-include("../include/roh_headers.hrl").

-callback handle_cast({start, Task :: #task{}, Sender :: pid()}, State :: term()) -> {stop, Result :: term(), State :: term()}.

