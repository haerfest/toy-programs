-module(future).
-export([future/1, await/1, await/2, demo/0]).

future(Fn) ->
    Caller = self(),
    spawn(fun() -> Caller ! {future, self(), Fn()} end).

await(Future) ->
    receive
        {future, Future, Result} ->
            Result
    end.

await(Future, Timeout) ->
    receive
        {future, Future, Result} ->
            Result
    after Timeout ->
            timeout
    end.

demo() ->
    io:format("starting a 5 second future~n", []),
    Future = future(fun() -> timer:sleep(5000), ok end),
    io:format("waiting a second...", []), 
    FirstResult = await(Future, 1000),
    io:format("got ~p~n", [FirstResult]),
    io:format("waiting until computed...", []),
    SecondResult = await(Future),
    io:format("got ~p~n", [SecondResult]).
    
    
