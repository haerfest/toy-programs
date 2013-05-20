% Estimate pi by throwing darts at a quarter of a dart board
% with radius 1.
%
% Example usage:
%
% > c(pi).
% > Pid = pi:start().
% > pi:estimate(Pid, 1000000).
% 3.1414

-module(pi).
-compile(export_all).

throw_dart() ->
  X = random:uniform(),
  Y = random:uniform(),
  X * X + Y * Y =< 1.

throw_darts(0) ->
  [];
throw_darts(Count) when Count > 0 ->
  Hit = throw_dart(),
  [Hit | throw_darts(Count - 1)].

estimate_pi(ThrowCount) when ThrowCount > 0 ->
  Throws   = throw_darts(ThrowCount),
  Hits     = [X || X <- Throws, X],
  HitCount = length(Hits),
  4 * HitCount / ThrowCount.

estimator() ->
  receive
    {From, ThrowCount} ->
      Pi = estimate_pi(ThrowCount),
      From ! Pi
  end.

start() ->
  spawn(?MODULE, estimator, []).

estimate(Pid, ThrowCount) ->
  Pid ! {self(), ThrowCount},
  receive
    Pi -> Pi
  end.
