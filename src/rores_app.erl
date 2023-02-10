%%%-------------------------------------------------------------------
%% @doc rores public API
%% @end
%%%-------------------------------------------------------------------

-module(rores_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rores_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
