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
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 9527, [binary, {packet, 0}, {active, true}]),
    gen_tcp:send(Socket, "A message"),
    gen_tcp:close(Socket).