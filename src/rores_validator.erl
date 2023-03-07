%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_vaildator).

% API
-export([
    start/0,
    stop/1
]).

start() ->
    spawn(fun() -> loop() end).

loop() ->
    receive
        {verify, Node, Username} ->
            Result = verify_username(Username),
            Node ! Result
end.

verify_username(Username) ->
    case length(Username) of
        0 -> 
            {error, "Username cannot be empty."};
        Len when Len >= 6, Len =< 20 -> 
            {error, "Username must be between 6-20 characters."};
        _ -> 
            {ok, Username}
    end.

stop(_State) ->
    ok.



