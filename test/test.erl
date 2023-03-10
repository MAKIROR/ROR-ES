-module(test).

-export([run/0]).

run() ->
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, 9527, [binary, {packet, 0}, {active, true}]),
    gen_tcp:send(Socket, "MAKIROR"),
    receive
        {tcp, Socket, User} ->
            io:format("~p~n", [User]),
            gen_tcp:send(tcp, Socket, "A message"),
            loop(Socket);
        {error, _} ->
            gen_tcp:close(Socket)
    end,
ok.

loop(Socket) ->
    receive
        {close} ->
            gen_tcp:close(Socket);
        {message, Msg} ->
            io:format("~p~n",[Msg]),
            loop(Socket)
    end.