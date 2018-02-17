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

-module(pi).
-export([estimate/1, estimate/2, thrower/2]).

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
  if
    Remaining == 0 ->
      % All darts thrown.
      await(Throwers, Count);
    Remaining >= PerThrower ->
      % Start another thrower.
      Thrower = spawn(?MODULE, thrower, [PerThrower, self()]),
      estimate(Count, Remaining - PerThrower, PerThrower, [Thrower|Throwers]);
    true ->
      % Final thrower may have fewer darts Remaining to throw.
      Thrower = spawn(?MODULE, thrower, [Remaining, self()]),
      estimate(Count, 0, PerThrower, [Thrower|Throwers])
  end.
