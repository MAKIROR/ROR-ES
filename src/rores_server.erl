%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_server).
-behaviour(gen_server).

% API
-export([
    start_link/3,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {clients=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, ValidatorName, ServerName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, ValidatorName, ServerName], []).

init([Port, ValidatorName, ServerName]) ->
    {ok, Listener} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, once}]),
    net_kernel:start([ServerName]),
    case net_adm:ping(ValidatorName) of
        pong ->
            io:format("Chat server started: ~p~n", [self()]),
            spawn(fun() -> acceptor(Listener, ValidatorName) end);
        pang ->
            io:format("Unable connect to validator~n", [])
    end,
    {ok, #state{clients = []}}.

acceptor(Listener, ValidatorName) -> 
    {ok, Socket} = gen_tcp:accept(Listener),
    {ok, {Address, Port}} = inet:peername(Socket),
    io:format("New connection: ~s:~p ~n",[inet:ntoa(Address), Port]),
    spawn(fun() -> acceptor(Listener, ValidatorName) end),
    handle_connection(Socket, ValidatorName).

handle_connection(Socket,ValidatorName) ->
    receive
        {ok, Username} ->
            case roc:call(ValidatorName, rores_vaildator, verify_username, [Username]) of
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

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

