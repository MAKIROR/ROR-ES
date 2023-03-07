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
    {ok, Listener} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, false}, {ip, Host}]),
    {ok, Validator} = net_kernel:connect_node('rores_validator@' ++ Host),
    spawn(fun() -> acceptor(Listener, Validator) end).

acceptor(Listener, Validator) -> 
    {ok, Socket} = gen_tcp:accept(Listener),
    {ok, {Address, Port}} = inet:peername(Socket),
    io:format("New connection: ~s:~p ~n",[inet:ntoa(Address), Port]),
    spawn(fun() -> acceptor(Listener, Validator) end),
    handle_connection(Socket, Validator).

handle_connection(Socket, Validator) ->
    receive
        {ok, Username} ->
            Validator ! {verify, self(), Username},
            receive
                {ok, _} ->
                    io:format("Verification passed~n"),
                    Socket ! {ok},
                    receive_msg(Socket, Username);
                {error, Reason} ->
                    io:format("Verification failed: ~p~n", [Reason]),
                    Socket ! {error, verification_failed, Reason}
            end;
        {error, Reason} ->
            io:format("Error receiving message: ~p~n", [Reason])
    end.

receive_msg(Socket, Username) ->
    case gen_tcp:recv(Socket, 0) of
        {error, Reason} ->
            io:format("Error receiving message: ~p~n", [Reason]);
        {ok, stop} ->
            io:format("Close a client connection ~n");
        {ok, Data} ->
            io:format("Got a new message from ~p: ~p ~n",[Username, Data]),
            receive_msg(Socket, Username)
    end.