-module(fibonacci).
-export([recursive/1, memoized/1]).

%% ----------------------------------------------------------------------------
%%  Execution times in microseconds (first element of returned tuple):
%%
%%  > timer:tc(fibonacci, recursive, [40]).
%%  {6390387,165580141}
%%
%% > timer:tc(fibonacci, memoized, [40]).
%% {480,165580141}
%% > timer:tc(fibonacci, memoized, [40]).
%% {6,165580141}
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%%  Trivial recursive definition.
%% ----------------------------------------------------------------------------

recursive(0) -> 1;
recursive(1) -> 1;
recursive(N) when N > 1 ->
    recursive(N - 1) + recursive(N - 2).

%% ----------------------------------------------------------------------------
%%  Memoized version.  It uses its own process dictionary to remember the
%%  (N, fib(N)) tuples it has calculated so far.
%% ----------------------------------------------------------------------------

memoized(N) when N >= 0 ->
    put(0, 1),
    put(1, 1),
    fib(N).

%% ----------------------------------------------------------------------------
%%  Little helper function that uses the process dictionary to calculate a
%%  fibonacci(N) only once for a given N.
%% ----------------------------------------------------------------------------

fib(N) ->
    case get(N) of
        undefined ->
            M = fib(N - 1) + fib(N - 2),
            put(N, M),
            M;
        M ->
            M
    end.
