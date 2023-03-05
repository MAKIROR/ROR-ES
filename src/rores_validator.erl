%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_server).

% API
-export([
        start/1,
        start/0
        ]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}]).
-define(DEFAULT_PORT, 9527).

%%%===================================================================
%%% API
%%%===================================================================

start(Port) ->
    {ok, Listener} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]),
    spawn(fun() -> acceptor(Listener) end).

start() ->
    start(?DEFAULT_PORT).

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
                    %todo
                    io:format("~p joins the chat room: ~n", [Username]),
                    receive_msg(Socket);
                false ->
                    io:format("Invalid username: ~p~n", [Username])
            end;
        {error, Reason} ->
            io:format("Error receiving message: ~p~n", [Reason])
    end.

receive_msg(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            % todo
        {error, Reason} ->
            {error, Reason}
    end.

verify_username(Username) ->
    case byte_size(Username) of
        N when N > 0 -> true;
        _ -> false
    end.



