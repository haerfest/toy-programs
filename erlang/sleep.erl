-module(sleep).
-export([demo/0, sink/0]).

sink() ->
    receive
        _ ->
            timer:sleep(5000),
            sink()
    end.

demo() ->
    register(sink, spawn(?MODULE, sink, [])),
    demo(0).
demo(Count) ->
    io:format("messages sent: ~p~c", [Count, 13]),
    sink ! message,
    demo(Count + 1).
