%%%-------------------------------------------------------------------
%% @doc rores public API
%% @end
%%%-------------------------------------------------------------------

-module(rores_app).

-behaviour(application).

-export([
     start/2,
     stop/1
     ]).

start(_Type, _StartArgs) ->
    case file:consult("config/server.config") of
        {ok, [{hostname, Host}, {port, Port}]} ->
            rores_sup:start_link(Host, Port),
            ok;
        {error, Reason} ->
            io:format("Failed to read config file: ~p~n", [Reason]),
            rores_sup:start_link("localhost"),
            ok
    end.

stop(_State) ->
    ok.

%% internal functions
