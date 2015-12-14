-module(philosophers).
-export([dine/0]).

% ------------------------------------------------------------------------------
%  The dinner table consists of philosophers and their shared forks.
% ------------------------------------------------------------------------------

-define(DINNERTABLE, #{plato     => {'red fork',    'blue fork'},
                       confucius => {'blue fork',   'green fork'},
                       socrates  => {'green fork',  'yellow fork'},
                       voltaire  => {'yellow fork', 'purple fork'},
                       descartes => {'purple fork', 'red fork'}}).

dine() ->
   create_forks(),
   create_philosophers(),
   ok.

% ------------------------------------------------------------------------------
%  Philosophers.
% ------------------------------------------------------------------------------

philosopher(Philosopher) ->
    randomize_behavior(),
    case shuffle({eat, think}) of
        {eat, _}   -> try_eat(Philosopher);
        {think, _} -> think(Philosopher)
    end.

try_eat(Philosopher) ->
    case acquire_forks(Philosopher) of
        success -> eat(Philosopher);
        failure -> think(Philosopher)
    end.

eat(Philosopher) ->
    log(Philosopher, eating),
    busy(),
    release_forks(Philosopher),
    think(Philosopher).

think(Philosopher) ->
    log(Philosopher, thinking),
    busy(),
    try_eat(Philosopher).

acquire_forks(Philosopher) ->
    {FirstFork, SecondFork} = shuffle(forks(Philosopher)),
    case acquire_fork(Philosopher, FirstFork) of
        success ->
            case acquire_fork(Philosopher, SecondFork) of
                success ->
                    success;
                failure ->
                    release_fork(FirstFork),
                    failure
            end;
        failure ->
            failure
    end.

release_forks(Philosopher) ->
    {FirstFork, SecondFork} = shuffle(forks(Philosopher)),
    release_fork(FirstFork),
    release_fork(SecondFork).

acquire_fork(Philosopher, Fork) ->
    Fork ! {acquire, Philosopher},
    receive Result -> Result end.

release_fork(Fork) ->
    Fork ! release.

% ------------------------------------------------------------------------------
%  Forks.
% ------------------------------------------------------------------------------

fork(Fork) ->
    fork(Fork, available).

fork(Fork, available) ->
    receive
        {acquire, Philosopher} ->
            Philosopher ! success,
            fork(Fork, inuse)
    end;

fork(Fork, inuse) ->
    receive
        {acquire, Philosopher} ->
            Philosopher ! failure,
            fork(Fork, inuse);
        release ->
            fork(Fork, available)
    end.

% ------------------------------------------------------------------------------
%  Create a process for each philosopher and each fork.
% ------------------------------------------------------------------------------

create_forks() ->
    F = fun({LeftFork, RightFork}, Forks) ->
        [LeftFork, RightFork | Forks]
    end,
    Forks = unique(lists:foldl(F, [], maps:values(?DINNERTABLE))),
    [register(Fork, spawn(fun() -> fork(Fork) end)) || Fork <- Forks].

create_philosophers() ->
    Philosophers = maps:keys(?DINNERTABLE),
    [register(Philosopher, spawn(fun() -> philosopher(Philosopher) end))
     || Philosopher <- Philosophers].

% ------------------------------------------------------------------------------
%  Small helper functions.
% ------------------------------------------------------------------------------

randomize_behavior() ->
    random:seed(erlang:phash2([node()]),
                erlang:monotonic_time(),
                erlang:unique_integer()).

forks(Philosopher) ->
    maps:get(Philosopher, ?DINNERTABLE).

busy() ->
    Delay = 500 + random:uniform(4500),
    timer:sleep(Delay).

log(Philosopher, Task) ->
    io:format("~s is ~s...~n", [Philosopher, Task]).

shuffle({A, B}) ->
    case random:uniform(2) of
        1 -> {A, B};
        2 -> {B, A}
    end.

unique(List) ->
    sets:to_list(sets:from_list(List)).
