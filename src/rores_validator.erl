%%%-------------------------------------------------------------------
%%% @author Makiror
%%% Created : February 2023
%%%-------------------------------------------------------------------

-module(rores_validator).
-behavior(gen_server).

% API
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("Validator started.~n"),
    {ok, []}.

handle_call({verify, Username}, Node, State) ->
    Result = verify_username(Username),
    net_kernel:send(Node, {reply, Result}),
    {reply, ok, State}.

verify_username(Username) ->
    case length(Username) of
        0 -> 
            {error, "Username cannot be empty."};
        Len when Len >= 6, Len =< 20 -> 
            {ok, Username};
        _ -> 
            {error, "Username must be between 6-20 characters."}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.