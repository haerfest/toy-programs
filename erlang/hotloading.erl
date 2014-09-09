-module(hotloading).
-export([loop/0]).

%% Change this to check which version of the module is loaded.
-define(version, 1).

loop() ->
    receive
        upgrade ->
            io:format("loading latest version of module ~p~n", [?MODULE]),
            code:purge(?MODULE),
            code:load_file(?MODULE),
            ?MODULE:loop();
        Message ->
            io:format("version ~p received: ~p~n", [?version, Message]),
            loop()
    end.

%% Example usage:
%%
%% 1) In an erl instance:
%%
%%    1> c(hotloading).
%%    {ok,hotloading}
%%    2> Loop = spawn(hotloading, loop, []).
%%    <0.47.0>
%%    3> Loop ! hi.
%%    version 1 received: hi
%%    hi
%%
%% 2) Change the version macro to 2 and save the file.
%%
%% 3) Compile the module in another erl instance:
%%
%%    1> c(hotloading)
%%    {ok,hotloading}
%%
%% 4) And finally in the first instance:
%%
%%    4> Loop ! upgrade.
%%    loading latest version of module hotloading
%%    upgrade
%%    5> Loop ! hi.
%%    version 2 received: hi
%%    hi
