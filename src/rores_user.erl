-module(user_server).
-export([start/0]).

start() ->
    NodeName = 'rores_user@localhost',
    Cookie = 'rores_user_cookie',
    erlang:register(rores_user, spawn(fun() -> loop() end)),
    net_kernel:start([NodeName, Cookie]),
    application:start(rores_user).

loop() ->
    receive
        _ -> loop()
    end.