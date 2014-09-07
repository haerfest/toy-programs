-module(messenger).
-export([start_server/0,
         server/1,
         logon/1,
         logoff/0,
         message/2,
         client/2]).

%% The node that will run the server.
server_node() ->
    messenger@kilkerran.

%% Server message loop.
server(User_List) ->
    receive
        {From, logon, Name} ->
            New_User_List = server_logon(From, Name, User_List),
            server(New_User_List);
        {From, logoff} ->
            New_User_List = server_logoff(From, User_List),
            server(New_User_List);
        {From, message_to, To, Message} ->
            server_transfer(From, To, Message, User_List),
            io:format("list is now: ~p~n", [User_List]),
            server(User_List)
    end.

%% Starts a server.
start_server() ->
    register(messenger, spawn(messenger, server, [[]])).

%% Registers a new client at the server.
server_logon(From, Name, User_List) ->
    case lists:keymember(Name, 2, User_List) of
        true ->
            From ! {messenger, stop, user_exists_at_other_node},
            User_List;
        false ->
            From ! {messenger, logged_on},
            [{From, Name} | User_List]
    end.

%% Unregisters a client at the server.
server_logoff(From, User_List) ->
    lists:keydelete(From, 1, User_List).

%% Checks whether the sender of a message is registered before transferring
%% a message.
server_transfer(From, To, Message, User_List) ->
    case lists:keysearch(From, 1, User_List) of
        false ->
            From ! {messenger, stop, you_are_not_logged_on};
        {value, {From, Name}} ->
            server_transfer(From, Name, To, Message, User_List)
    end.

%% Transfers a message to another user.
server_transfer(From, Name, To, Message, User_List) ->
    case lists:keysearch(To, 2, User_List) of
        false ->
            From ! {messenger, receiver_not_found};
        {value, {ToPid, To}} ->
            ToPid ! {message_from, Name, Message},
            From ! {messenger, sent}
    end.

%% User command to logon.
logon(Name) ->
    case whereis(mess_client) of
        undefined ->
            register(mess_client,
                     spawn(messenger, client, [server_node(), Name]));            
        _ -> already_logged_on
    end.

%% User command to logoff.
logoff() ->
    mess_client ! logoff.

%% User command to send a message to a user.
message(ToName, Message) ->
    case whereis(mess_client) of
        undefined ->
            not_logged_on;
        _ ->
            mess_client ! {message_to, ToName, Message},
            ok
    end.

%% Registers itself as a new client with a server.
client(Server_Node, Name) ->
    {messenger, Server_Node} ! {self(), logon, Name},
    await_result(),
    client(Server_Node).

%% Client message loop.
client(Server_Node) ->
    receive
        logoff ->
            {messenger, Server_Node} ! {self(), logoff},
            exit(normal);
        {message_to, ToName, Message} ->
            {messenger, Server_Node} ! {self(), message_to, ToName, Message},
            await_result();
        {message_from, FromName, Message} ->
            io:format("message from ~p: ~p~n", [FromName, Message])
    end,    
    client(Server_Node).

%% Waits for a message from the server and prints it.
await_result() ->
    receive
        {messenger, stop, Why} ->
            io:format("~p~n", [Why]),
            exit(normal);
        {messenger, What} ->
            io:format("~p~n", [What])
    end.
