%%%-------------------------------------------------------------------
%%% @author Makiror
%%% @doc
%%%
%%% @end
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_server).

%% API
-export([
        start/1,
        start/0,
        acceptor/1
        ]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}]).
-define(DEFAULT_PORT, 9527).

%%%===================================================================
%%% API
%%%===================================================================

start(Port) ->
    {ok, Listener} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(fun() -> acceptor(Listener) end).

start() ->
    start(?DEFAULT_PORT).

acceptor(Listener) -> 
    {ok, Socket} = gen_tcp:accept(Listener),
    spawn(fun() -> acceptor(Listener) end),
    handle_client(Socket).

handle_client(Socket) ->
    receive
        {tcp,Socket}->
            handle_client(Socket);
        {tcp_quit, Socket} ->
            io:format("Close the client connection ~n");
        Msg ->
            io:format("got a new message: ~p ~n",[Msg]),
            handle_client(Socket)
    end.



