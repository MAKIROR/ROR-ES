%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_validator).

% API
-export([
    start/1,
    verify_username/1
]).

%todo

start(ValidatorName) ->
    {ok, Pid} = net_kernel:start([ValidatorName]),
    io:format("Validator started: ~p~n", [Pid]).

verify_username(Username) ->
    case length(Username) of
        0 -> 
            {error, "Username cannot be empty."};
        Len when Len >= 6, Len =< 20 -> 
            {ok, Username};
        _ -> 
            {error, "Username must be between 6-20 characters."}
    end.
