%%%-------------------------------------------------------------------
%% @doc rores top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rores_sup).

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Host, Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Host, Port]).

init([Host, Port]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [    
        {validator, {rores_validator, start, []}, permanent, 5000, worker, [rores_vaildator]},
        {server, {rores_server, start, [Host, Port]}, permanent, 5000, worker, [rores_server]}
    ],
    {ok, {SupFlags, ChildSpecs}}.