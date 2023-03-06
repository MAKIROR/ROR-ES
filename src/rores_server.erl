%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_server).

% API
-export([
    start/2
]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}]).

%%%===================================================================
%%% API
%%%===================================================================

start(Host, Port) ->
    {ok, Listener} = gen_tcp:listen(Host, Port, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]),
    spawn(fun() -> acceptor(Listener) end).

acceptor(Listener) -> 
    {ok, Socket} = gen_tcp:accept(Listener),
    {ok, {Address, Port}} = inet:peername(Socket),
    io:format("New connection: ~s:~p ~n",[inet:ntoa(Address), Port]),
    spawn(fun() -> acceptor(Listener) end),
    handle_connection(Socket).

handle_connection(Socket) ->
    receive
        {ok, Username} ->
            case verify_username(Username) of
                true ->
                    io:format("~p Joins the chat room: ~n", [Username]),
                    receive_msg(Socket);
                false ->
                    io:format("Invalid username: ~p~n", [Username])
            end;
        {error, Reason} ->
            io:format("Error receiving message: ~p~n", [Reason])
    end.

receive_msg(Socket) ->
    case gen_tcp:recv(Socket, 4) of
        {ok, stop} ->
            io:format("Close a client connection ~n");
        {ok, Data} ->
            io:format("Got a new message: ~p ~n",[Msg]),
            receive_msg(Socket)
        {error, Reason} ->
            %todo

    end.