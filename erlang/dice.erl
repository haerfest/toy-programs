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
    histogram(Rolls, #{1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0}).
histogram([Roll|Rest], Histo) ->
    {ok, Count} = maps:find(Roll, Histo),
    Histo2 = maps:update(Roll, Count+1, Histo),
    histogram(Rest, Histo2);
histogram([], Histo) ->
    Histo.

probabilities(Histo) ->
    Rolls = lists:sum(maps:values(Histo)),
    maps:map(fun(_,V) -> V/Rolls end, Histo).
                     
