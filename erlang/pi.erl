% Multicore estimation of Pi.
%
% Throws darts at a quarter unit circle and estimates Pi by the ratio between
% the number of darts that fall in the quarter circle and the total number of
% darts thrown.
%
% Example usage:
%  $ erl
%  1> c(pi).
%  2> pi:estimate(100000000).
%  3.14172076
%
% To time (Core i5, 2 cores + hyper-threading):
%  3> pi:time(fun() -> pi:estimate(10000000, 1) end).
%  runtime ...... 5642000 usec
%  wall_clock ... 5754000 usec
%  3.1422768
%  4> pi:time(fun() -> pi:estimate(10000000, 2) end).
%  runtime ...... 5648000 usec
%  wall_clock ... 2894000 usec
%  3.1416596
%  5> pi:time(fun() -> pi:estimate(10000000, 4) end).
%  runtime ...... 11322000 usec
%  wall_clock ... 2953000 usec
%  3.1410572

-module(pi).
-export([estimate/1, estimate/2, time/1, thrower/2]).

time(Function) ->
  statistics(runtime),
  statistics(wall_clock),

  Result = Function(),

  {_, Time1} = statistics(runtime),
  {_, Time2} = statistics(wall_clock),
  U1 = Time1 * 1000,
  U2 = Time2 * 1000,
  io:format("runtime ...... ~p usec~n", [U1]),
  io:format("wall_clock ... ~p usec~n", [U2]),

  Result.

thrower(ThrowCount, Requestor) ->
  % A thrower is modelled as an Erlang process.
  thrower(ThrowCount, 0, Requestor).

thrower(0, HitCount, Requestor) ->
  % No more darts to throw, report the number of hits.
  Requestor ! {self(), HitCount};
thrower(ThrowCount, HitCount, Requestor) ->
  % Throw another dart at the quarter unit circle.
  X = rand:uniform(),
  Y = rand:uniform(),
  Radius = X*X + Y*Y,
  if
    Radius < 1 -> thrower(ThrowCount - 1, HitCount + 1, Requestor);
    true       -> thrower(ThrowCount - 1, HitCount,     Requestor)
  end.

await(Throwers, ThrowCount) ->
  % Wait for all throwers to finish throwing.
  await(Throwers, ThrowCount, 0).

await([], ThrowCount, HitCount) ->
  % Estimate Pi once all throwers are done.
  4 * HitCount / ThrowCount;
await([Thrower|Throwers], ThrowCount, HitCount) ->
  % Wait for another thrower to finish.
  receive
    {Thrower, ThrowerHitCount} ->
      await(Throwers, ThrowCount, HitCount + ThrowerHitCount)
  end.

estimate(ThrowCount) ->
  % Start a thrower for each CPU core.
  ThrowerCount = erlang:system_info(schedulers_online),
  estimate(ThrowCount, ThrowerCount).

estimate(ThrowCount, ThrowerCount) ->
  % Start a specified number of throwers and divide the work.
  rand:seed(exs64),
  ThrowsPerThrower = ceil(ThrowCount / ThrowerCount),
  estimate(ThrowCount, ThrowCount, ThrowsPerThrower, []).

estimate(Count, Remaining, PerThrower, Throwers) ->
  % Spawn throwers to throw darts and wait for them to complete.
  if
    Remaining == 0 ->
      % All darts thrown.
      await(Throwers, Count);
    Remaining >= PerThrower ->
      % Start another thrower.
      Thrower = spawn(?MODULE, thrower, [PerThrower, self()]),
      estimate(Count, Remaining - PerThrower, PerThrower, [Thrower|Throwers]);
    true ->
      % Final thrower may have fewer darts left to throw.
      Thrower = spawn(?MODULE, thrower, [Remaining, self()]),
      estimate(Count, 0, PerThrower, [Thrower|Throwers])
  end.
