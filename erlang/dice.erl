-module(dice).
-export([start/1, stop/1, roll/1, roll/2, loop/0]).

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
