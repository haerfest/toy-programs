-module(future).
-export([future/1, future/2, await/1, await/2, demo/1, worker/2]).

% Invokes a function and sends the result back to the caller.
worker(Fn, Caller) ->
    Caller ! {future, self(), Fn()}.

% Executes a future on the local node.
future(Fn) ->
    spawn(worker, [Fn, self()]).

% Executes a future on any node.
future(Node, Fn) ->
    spawn(Node, ?MODULE, worker, [Fn, self()]).

% Waits for a future to complete.
await(Future) ->
    receive
        {future, Future, Result} ->
            Result
    end.

% Waits until a timeout for a future to complete.
await(Future, Timeout) ->
    receive
        {future, Future, Result} ->
            Result
    after Timeout ->
            timeout
    end.

% Demonstrates running and waiting for futures.
demo(Node) ->
    io:format("starting a 5 second future on node ~p~n", [Node]),
    Future = future(Node, fun() -> timer:sleep(5000), ok end),

    io:format("waiting a second...", []), 
    FirstResult = await(Future, 1000),
    io:format("got ~p~n", [FirstResult]),

    io:format("waiting until computed...", []),
    SecondResult = await(Future),
    io:format("got ~p~n", [SecondResult]).
    
    
