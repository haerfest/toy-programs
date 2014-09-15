-module(dice).
-export([start/1, stop/1, roll/1, roll/2, histogram/1, probabilities/1, loop/0]).

loop() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    receive
        {From, roll, Dice} ->
            From ! {self(), [random:uniform(6) || _ <- lists:seq(1, Dice)]},
            loop();
        _ ->
            stop
    end.
            
start(Node) ->
    spawn(Node, ?MODULE, loop, []).

stop(Server) ->
    Server ! stop.

roll(Server) ->
    Server ! {self(), roll, 1},
    receive
        {Server, [Roll]} ->
            Roll
    end.

roll(Server, N) ->
    Server ! {self(), roll, N},
    receive
        {Server, Rolls} ->
                Rolls
    end.

histogram(Rolls) ->
    histogram(Rolls, [0, 0, 0, 0, 0, 0]).
histogram([Roll|Rest], Histo) ->
    Histo2 = lists:sublist(Histo, Roll-1) ++ [lists:nth(Roll, Histo)+1] ++ lists:nthtail(Roll, Histo),
    histogram(Rest, Histo2);
histogram([], Histo) ->
    Histo.

probabilities(Rolls) ->
    Sum = lists:sum(Rolls),
    lists:map(fun(X) -> X/Sum end, Rolls).
