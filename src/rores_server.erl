%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_server).
-behaviour(gen_server).

% API
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    start_link/0
]).

-record(state, {clients=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([Host, Port]) ->
    {ok, Listener} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, false}, {ip, Host}]),
    {ok, Validator} = net_kernel:connect_node('rores_validator@' ++ Host),
    spawn(fun() -> acceptor(Listener, Validator) end),
    {ok, #state{clients = []}}.

acceptor(Listener, Validator) -> 
    {ok, Socket} = gen_tcp:accept(Listener),
    {ok, {Address, Port}} = inet:peername(Socket),
    io:format("New connection: ~s:~p ~n",[inet:ntoa(Address), Port]),
    spawn(fun() -> acceptor(Listener, Validator) end),
    handle_connection(Socket, Validator).

handle_connection(Socket, Validator) ->
    receive
        {ok, Username} ->
            Ref = erlang:monitor(process, Validator),
            Validator ! {{verify, Username}, self(), Ref},
            receive
                {ok, _} ->
                    io:format("Verification passed~n"),
                    Socket ! {ok},
                    receive_msg(Socket, Username);
                {error, Reason} ->
                    io:format("Verification failed: ~p~n", [Reason]),
                    Socket ! {error, verification_failed, Reason}
            after 5000 ->
                    io:format("Authentication timeout~n"),
                    Socket ! {error, verification_failed, timeout}
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

