-module(im_client_registry).

-include_lib("eunit/include/eunit.hrl").

-export( [ new/0, add_client/3, get_client/2, get_pid/2, del_pid/2, clients/1, pids/1 ] ).

new() ->
    ClientPIDs = dict:new(),
    PIDClients = dict:new(),
    { ClientPIDs, PIDClients }.

add_client( Name, PID, Registry ) ->
    { ClientPIDs, PIDClients } = Registry,
    CP = dict:store( Name, PID, ClientPIDs ),
    PC = dict:store( PID, Name, PIDClients ),
    { CP, PC }.

get_client( PID, Registry ) ->
    { _, PIDClients } = Registry,
    Found = dict:find( PID, PIDClients ),
    case Found of
        error ->
            io:format( "~w no client for PID ~w - not registered~n", [ self(), PID ] ),
            unknown;
        { ok, Name } ->
            Name
    end.


get_pid( Name, Registry ) ->
    { ClientPIDs, _ } = Registry,
    Found = dict:find( Name, ClientPIDs ),
    case Found of
        error ->
            io:format( "~w no client for Name ~w - not registered~n", [ self(), Name ] ),
            unknown;
        { ok, PID } ->
            PID
    end.


del_pid( PID, Registry ) ->
    { ClientPIDs, PIDClients } = Registry,
    case get_client( PID, Registry ) of
        unknown ->
            io:format( "~w no client for PID ~w - not registered~n", [ self(), PID ] ),
            { CP, PC } = { ClientPIDs, PIDClients };
        Name ->
            CP = dict:erase( Name, ClientPIDs ),
            PC = dict:erase( PID, PIDClients )
    end,
    { CP, PC }.

clients( Registry ) ->
    { ClientPIDs, _ } = Registry,
    dict:fetch_keys( ClientPIDs ).

pids( Registry ) ->
    { _, PIDClients } = Registry,
    dict:fetch_keys( PIDClients ).


% UNIT TESTS

new_test_() ->
    Registry = new(),
    [] = clients( Registry ),
    [] = pids( Registry ),
    [
        ?_assertEqual( unknown, get_client( foo, Registry ) ),
        ?_assertEqual( unknown, get_pid( self(), Registry ) )
    ].

single_test_() ->
    MyPID = self(),
    MyName = foo,
    BlankReg = new(),
    Registry = add_client( MyName, MyPID, BlankReg ),
    [
        ?_assertEqual( [ MyName ], clients( Registry ) ),
        ?_assertEqual( [ MyPID ], pids( Registry ) ),
        ?_assertEqual( MyName, get_client( MyPID, Registry ) ),
        ?_assertEqual( MyPID, get_pid( MyName, Registry ) ),
        ?_assertEqual( unknown, get_client( bar, Registry ) ),
        ?_assertEqual( unknown, get_pid( bogus, Registry ) )
    ].


multi_test_() ->
    BlankReg = new(),
    RegistryOne = add_client( foo, bogusPIDOne, BlankReg ),
    RegistryTwo = add_client( bar, bogusPIDTwo, RegistryOne ),
    RegistryThree = add_client( baz, bogusPIDThree, RegistryTwo ),
    PIDs = pids( RegistryThree ),
    Names = clients( RegistryThree ),
    [
        ?_assert( lists:member( foo, Names ) ),
        ?_assert( lists:member( bar, Names ) ),
        ?_assert( lists:member( baz, Names ) ),
        ?_assert( lists:member( bogusPIDOne, PIDs ) ),
        ?_assert( lists:member( bogusPIDTwo, PIDs ) ),
        ?_assert( lists:member( bogusPIDThree, PIDs ) ),
        ?_assertNot( lists:member( bogus, Names ) ),
        ?_assertNot( lists:member( bogusPID, PIDs ) ),
        ?_assertEqual( foo, get_client( bogusPIDOne, RegistryThree ) ),
        ?_assertEqual( bogusPIDOne, get_pid( foo, RegistryThree ) ),
        ?_assertEqual( bar, get_client( bogusPIDTwo, RegistryThree ) ),
        ?_assertEqual( bogusPIDTwo, get_pid( bar, RegistryThree ) ),
        ?_assertEqual( baz, get_client( bogusPIDThree, RegistryThree ) ),
        ?_assertEqual( bogusPIDThree, get_pid( baz, RegistryThree ) )
    ].

