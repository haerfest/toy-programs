-module(randomreddit).
-compile(export_all).

start() ->
    inets:start().

stop() ->
    inets:stop().

random_sub() ->
    Request = {"http://www.reddit.com/r/random/", []},
    HttpOptions = [{autoredirect, false}],
    Options = [],
    {ok, {{_HttpVersion, 302, _Reason}, Headers, _Body}} = httpc:request(get, Request, HttpOptions, Options),
    proplists:get_value("location", Headers).

list(0) ->
    ok;
list(N) when N > 0 ->
    Location = random_sub(),
    io:format("~s~n", [Location]),
    RandomDelay = 1000 * random:uniform(3),
    timer:sleep(RandomDelay),
    list(N - 1).
    
    
    

    

