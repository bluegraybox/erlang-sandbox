-module(im).

-export([ start_server/0, spawn_server/0, server_loop/2, start_client/1, start_client/2, client_loop/1 ]).


spawn_server() ->
    ClientPIDs = dict:new(),
    PIDClients = dict:new(),
    ServerPID = spawn( im, server_loop, [ ClientPIDs, PIDClients ] ),
    register( im_server, ServerPID ).

start_server() ->
    ClientPIDs = dict:new(),
    PIDClients = dict:new(),
    register( im_server, self() ),
    server_loop( ClientPIDs, PIDClients ).

server_loop( ClientPIDs, PIDClients ) ->
    receive
        % FIXME: we should handle real exit signals
        stop ->
            io:format( "~w server exiting~n", [ self() ] );
        { sign_on, Name, PID } ->
            CP = dict:store( Name, PID, ClientPIDs ),
            PC = dict:store( PID, Name, PIDClients ),
            io:format( "~w adding client ~w with PID ~w~n", [ self(), Name, PID ] ),
            server_loop( CP, PC );
        { sign_off, PID } ->
            Client = dict:find( PID, PIDClients ),
            case Client of
                error ->
                    io:format( "~w no client for PID ~w - not registered~n", [ self(), PID ] ),
                    server_loop( ClientPIDs, PIDClients );
                { ok, Name } ->
                    CP = dict:erase( Name, ClientPIDs ),
                    PC = dict:erase( PID, PIDClients ),
                    server_loop( CP, PC )
            end;
        { send, FromPID, ToClient, Message } ->
            send_message( ClientPIDs, PIDClients, FromPID, ToClient, Message ),
            server_loop( ClientPIDs, PIDClients );
        dump ->
            io:format( "~w clients: ~s~n", [ self(), string:join( [ [ atom_to_list(K) ] || K <- dict:fetch_keys( ClientPIDs ) ], ", " ) ] ),
            im:server_loop( ClientPIDs, PIDClients );
        { spam, FromPID, Message } ->
            io:format( "~w spamming everyone~n", [ self() ] ),
            Spam = fun(ToClient) -> send_message( ClientPIDs, PIDClients, FromPID, ToClient, Message ) end,
            lists:foreach( Spam, dict:fetch_keys( ClientPIDs ) ),
            im:server_loop( ClientPIDs, PIDClients );
        reload ->
            io:format( "~w reloading code~n", [ self() ] ),
            Reload = fun(P) -> P ! reload end,
            lists:foreach( Reload, dict:fetch_keys( PIDClients ) ),
            im:server_loop( ClientPIDs, PIDClients );
        Unknown ->
            io:format( "~w got unknown message ~w~n", [ self(), Unknown ] ),
            server_loop( ClientPIDs, PIDClients )
    end.


send_message( ClientPIDs, PIDClients, FromPID, ToClient, Message ) ->
    To = dict:find( ToClient, ClientPIDs ),
    case To of
        error ->
            io:format( "~w no such client ~w~n", [ self(), ToClient ] ),
            FromPID ! { error, string:concat( "No such client: ", atom_to_list( ToClient ) ) };
        { ok, ToPID } ->
            From = dict:find( FromPID, PIDClients ),
            case From of
                error ->
                    io:format( "~w sender not registered: ~w~n", [ self(), FromPID ] ),
                    ToPID ! { message, FromPID, Message };
                { ok, FromClient } ->
                    io:format( "~w sending message from ~w to ~s(~w): ~s~n", [ self(), FromClient, ToClient, ToPID, Message ] ),
                    ToPID ! { message, FromClient, Message }
            end
    end.


start_client( Name ) ->
    ClientPID = spawn( im, client_loop, [ im_server ] ),
    register( Name, ClientPID ),
    im_server ! { sign_on, Name, ClientPID }.

start_client( Name, NodeName ) ->
    ClientPID = spawn( im, client_loop, [ { im_server, NodeName } ] ),
    register( Name, ClientPID ),
    { im_server, NodeName } ! { sign_on, Name, ClientPID }.

client_loop( Server ) ->
    receive
        % FIXME: we should handle real exit signals
        stop ->
            Server ! { sign_off, self() },
            io:format( "~w client exiting~n", [ self() ] );
        { error, ErrMsg } ->
            io:format( "~w got error while sending message: ~w~n", [ self(), ErrMsg ] ),
            client_loop( Server );
        { message, From, Response } ->
            io:format( "~w got new message from ~w: ~s~n", [ self(), From, Response ] ),
            client_loop( Server );
        { send, To, Message } ->
            Server ! { send, self(), To, Message },
            client_loop( Server );
        { spam, Message } ->
            Server ! { spam, self(), Message },
            client_loop( Server );
        dump ->
            io:format( "~w server: ~w~n", [ self(), Server ] ),
            client_loop( Server );
        reload ->
            io:format( "~w reloading code~n", [ self() ] ),
            im:client_loop( Server )
    end.

