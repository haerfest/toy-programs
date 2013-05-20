-module(pi).
-export([estimate/1]).

% Throws one dart at a quarter circle of radius one
% and returns whether the dart hit.
throw_dart() ->
  X = random:uniform(),
  Y = random:uniform(),
  X * X + Y * Y =< 1.

% Throws a number of darts at a quarter circle of radius
% one and returns a list where element i indicates whether
% throw i was a hit or not.
throw_darts(0) ->
  [];
throw_darts(Count) when Count > 0 ->
  Hit = throw_dart(),
  [Hit | throw_darts(Count - 1)].

% Estimates pi by throwing darts at a quarter circle
% of radius 1. The ratio between the darts that hit
% the quarter circle and those that don't, are an
% estimation of a quarter pi.
estimate(ThrowCount) when ThrowCount > 0 ->
  Throws   = throw_darts(ThrowCount),
  Hits     = [X || X <- Throws, X],
  HitCount = length(Hits),
  4 * HitCount / ThrowCount.
