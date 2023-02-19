%%%-------------------------------------------------------------------
%%% @author Makiror
%%% @doc
%%% Client module for test
%%% @end
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_client).

-export([connect/0]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}]).

connect()->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 9527, ?TCP_OPTIONS),
    gen_tcp:send(Socket, "A message"),
    gen_tcp:close(Socket).