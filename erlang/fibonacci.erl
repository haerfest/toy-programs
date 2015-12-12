-module(fibonacci).
-export([recursive/1, memoized/1]).

%% ----------------------------------------------------------------------------
%%  Execution times in microseconds (first element of returned tuple):
%%
%%  > timer:tc(fibonacci, recursive, [40]).
%%  {6390387,165580141}
%%
%%  > timer:tc(fibonacci, memoized, [40]).
%%  {214,165580141}
%% ----------------------------------------------------------------------------

%% ----------------------------------------------------------------------------
%%  Trivial recursive definition.
%% ----------------------------------------------------------------------------

recursive(0) -> 1;
recursive(1) -> 1;
recursive(N) when N > 1 ->
    recursive(N - 1) + recursive(N - 2).

%% ----------------------------------------------------------------------------
%%  Memoized version.  It spawns a separate process to maintain the state,
%%  i.e. the N => fibonacci(N) tuples we have calculated so far.
%% ----------------------------------------------------------------------------

memoized(N) when N >= 0 ->
    Memory = spawn(fun memory/0),
    store(Memory, 0, 1),
    store(Memory, 1, 1),
    M = helper(N, Memory),
    stop(Memory),
    M.

%% ----------------------------------------------------------------------------
%%  Little helper function that uses the memory process to calculate a
%%  fibonacci(N) only once for a given N.
%% ----------------------------------------------------------------------------

helper(N, Memory) ->
    case retrieve(Memory, N) of
        undefined ->
            M = helper(N - 1, Memory) + helper(N - 2, Memory),
            store(Memory, N, M),
            M;
        M ->
            M
    end.

%% ----------------------------------------------------------------------------
%%  The memory process that keeps track of calculated fibonacci numbers.  It
%%  uses a process's built-in dictionary to store them.  Note that the store
%%  is generic, it has no notion of what is being calculated nor that anything
%%  is calculated at all -- it only keeps track of (key, value) pairs.
%% ----------------------------------------------------------------------------

memory() ->
    receive
        {set, N, M} ->
            put(N, M),
            memory();
        {get, N, Pid} ->
            Pid ! get(N),
            memory();
        stop ->
            ok
    end.

%% ----------------------------------------------------------------------------
%%  Accessor functions to interact with the memory.
%% ----------------------------------------------------------------------------

store(Memory, N, M) -> Memory ! {set, N, M}.

retrieve(Memory, N) ->
    Memory ! {get, N, self()},
    receive M -> M end.

stop(Memory) -> Memory ! stop.
