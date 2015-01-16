-module(im).

-export([ start_server/0, server_loop/1, client/1, client/2, cli/1, cli/2, client_loop/1 ]).

-include_lib("eunit/include/eunit.hrl").

-import( im_client_registry, [ add_client/3, get_client/2, get_pid/2, del_pid/2, clients/1, pids/1 ] ).


start_server() ->
    Server = im_server,
    IsRegistered = lists:member( Server, registered() ),
    if
        IsRegistered ->
            io:format( "~w Server ~w already registered - exiting.~n", [ self(), Server ] );
        true ->
            Registry = im_client_registry:new(),
            ServerPID = spawn( im, server_loop, [ Registry ] ),
            register( Server, ServerPID )
    end.


server_loop( Registry ) ->
    receive
        {'EXIT', FromPID, Reason} ->
            % disconect all clients?
            io:format( "~w server exiting on signal from ~w: '~s'~n", [ self(), FromPID, Reason ] );
        stop ->
            io:format( "~w server exiting~n", [ self() ] );
        { sign_on, Name, PID } ->
            Reg = add_client( Name, PID, Registry ),
            io:format( "~w adding client ~w with PID ~w~n", [ self(), Name, PID ] ),
            server_loop( Reg );
        { sign_off, PID } ->
            case get_client( PID, Registry ) of
                unknown ->
                    io:format( "~w no client for PID ~w - not registered~n", [ self(), PID ] ),
                    server_loop( Registry );
                Name ->
                    io:format( "~w removing client ~w, pid ~w~n", [ self(), Name, PID ] ),
                    Reg = del_pid( PID, Registry ),
                    server_loop( Reg )
            end;
        { send, FromPID, ToClient, Message } ->
            send_message( Registry, FromPID, ToClient, Message ),
            server_loop( Registry );
        dump ->
            io:format( "~w clients: ~s~n", [ self(), string:join( [ [ atom_to_list(K) ] || K <- clients( Registry ) ], ", " ) ] ),
            im:server_loop( Registry );
        { spam, FromPID, Message } ->
            io:format( "~w spamming everyone~n", [ self() ] ),
            Spam = fun(ToClient) -> send_message( Registry, FromPID, ToClient, Message ) end,
            lists:foreach( Spam, clients( Registry ) ),
            im:server_loop( Registry );
        reload ->
            Reloaded = reload( im ),
            if
                Reloaded ->
                    Reload = fun(P) -> P ! reload end,
                    lists:foreach( Reload, pids( Registry ) ),
                    im:server_loop( Registry );
                true ->
                    server_loop( Registry )
            end;
        Unknown ->
            io:format( "~w server got unknown message ~w~n", [ self(), Unknown ] ),
            server_loop( Registry )
    end.


reload( Module ) ->
    Purged = code:soft_purge( Module ),
    if
        Purged ->
            case code:load_file( Module ) of
                {module, Module} ->
                    io:format( "~w reloading ~w code~n", [ self(), Module ] ),
                    true;
                {error, What} ->
                    io:format( "~w failed reloading code: ~w~n", [ self(), What ] ),
                    false
            end;
        true ->
            io:format( "~w could not purge code for ~w~n", [ self(), Module ] ),
            false
    end.


send_message( Registry, FromPID, ToClient, Message ) ->
    case get_pid( ToClient, Registry ) of
        unknown ->
            io:format( "~w no such client ~w~n", [ self(), ToClient ] ),
            FromPID ! { error, string:concat( "No such client: ", atom_to_list( ToClient ) ) };
        ToPID ->
            case get_client( FromPID, Registry ) of
                unknown ->
                    io:format( "~w sender not registered: ~w~n", [ self(), FromPID ] ),
                    ToPID ! { message, FromPID, Message };
                FromClient ->
                    io:format( "~w sending message from ~w to ~s(~w): ~s~n", [ self(), FromClient, ToClient, ToPID, Message ] ),
                    ToPID ! { message, FromClient, Message }
            end
    end.


client( Name ) ->
    start_client( Name, im_server ).


client( Name, NodeName ) ->
    start_client( Name, { im_server, NodeName } ).


start_client( Name, Server ) ->
    IsRegistered = lists:member( Server, registered() ),
    if
        IsRegistered ->
            ClientPID = spawn( im, client_loop, [ Server ] ),
            register( Name, ClientPID ),
            Server ! { sign_on, Name, ClientPID },
            Server;
        true ->
            io:format( "~w Server ~w not registered - exiting.~n", [ self(), Server ] )
    end.


client_loop( Server ) ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', FromPID, Reason} ->
            Server ! { sign_off, self() },
            io:format( "~w client exiting on signal from ~w: '~s'~n", [ self(), FromPID, Reason ] );
        stop ->
            Server ! { sign_off, self() },
            io:format( "~w client exiting~n", [ self() ] );
        { error, ErrMsg } ->
            io:format( "~w got error while sending message: ~w~n", [ self(), ErrMsg ] ),
            client_loop( Server );
        { message, From, Response } ->
            io:format( "~w got new message from ~w: '~s'~n", [ self(), From, Response ] ),
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
            Reloaded = reload( im ),
            if
                Reloaded ->
                    im:client_loop( Server );
                true ->
                    client_loop( Server )
            end;
        Unknown ->
            io:format( "~w client got unknown message ~w~n", [ self(), Unknown ] ),
            client_loop( Server )
    end.


shift_token( String ) ->
    Stripped = string:strip( String ),
    First = string:sub_word( Stripped, 1 ),
    StartRest = string:len( First ) + 1,
    RestRaw = string:sub_string( Stripped, StartRest ),
    Rest = string:strip( RestRaw ),
    [ First, Rest ].


shift_token_test_() ->
    [
        ?_assertEqual( [ "foo", "bar baz" ], shift_token( "foo bar baz" ) ),
        ?_assertEqual( [ "foo", "bar baz" ], shift_token( " foo bar baz" ) ),
        ?_assertEqual( [ "foo", "bar baz" ], shift_token( " foo     bar baz" ) ),
        ?_assertEqual( [ "foo", "bar baz" ], shift_token( " foo     bar baz   " ) ),
        ?_assertEqual( [ "foo", "bar    baz" ], shift_token( " foo     bar    baz   " ) )
    ].


cli( Client ) ->
    Server = client( Client ),
    cli_loop( Client, Server ).


cli( Client, ServerNode ) ->
    Server = client( Client, ServerNode ),
    cli_loop( Client, Server ).


cli_loop( Client, Server ) ->
    CmdText = io:get_line( "> " ),
    [ Cmd, Params ] = shift_token( CmdText ),
    case Cmd of
        "send" ->
            [ ToString, Message ] = shift_token( Params ),
            To = list_to_atom( ToString ),
            % Server ! { send, Params }
            io:format( "~w sending message to ~w: ~s~n", [ self(), To, Message ] ),
            cli_loop( Client, Server );
        "q" ->
            io:format( "~w exiting CLI~n", [ self() ] )
    end.



% UNIT TESTS!!


