%%%-------------------------------------------------------------------
%%% @author Makiror
%%% @doc
%%% Client module for test
%%% @end
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_client).

-export([connect/0]).

connect()->
    {ok, _} = gen_tcp:connect({127,0,0,1}, 9527, [binary, {packet, 4}, {active, true}]).
    %todo