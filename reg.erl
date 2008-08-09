-module(reg).

-include_lib("eunit/include/eunit.hrl").

% set_registry has to be exported, even though it's only called internally.  I don't understand...
-export([ start_registry/0, set_registry/1, start_node/1, lookup_test/0, list_test/0 ]).


start_registry() ->
    PID = whereis( registry ),
    if
        undefined == PID ->
            register( registry, spawn( reg, set_registry, [ [] ] ) );
        true ->
            PID
    end.

set_registry( Nodes ) ->
    io:format("~w registry wait loop~n", [ self() ]),
    receive
        { add, Name, PID } ->
            io:format("~w adding node ~w: ~w~n", [ self(), Name, PID ]),
            set_registry( [ { Name, PID } | Nodes ] );
        { lookup, Name, PID } ->
            io:format("~w got lookup request for node ~w from ~w~n", [ self(), Name, PID ]),
            Match = find_node( Name, Nodes ),
            PID ! { Match },
            set_registry( Nodes );
        { list_nodes, PID } ->
            [ io:format( "Node ~w: ~w~n", [ Name, Node_PID ] ) || { Name, Node_PID } <- Nodes ],
            PID ! { Nodes },
            set_registry( Nodes );
        { clear } ->
            io:format( "~w clearing registry~n", [ self() ] ),
            set_registry( [] );
        { stop } ->
            io:format( "~w exiting~n", [ self() ] )
    end.

% Get the PID for the named node.
find_node( TargetName, [] ) -> [];
find_node( TargetName, [ { Name, PID } | Rest ] ) when TargetName == Name -> PID;
find_node( TargetName, [ { Name, PID } | Rest ] ) -> find_node( TargetName, Rest ).
% Unit tests for same.
find_node_empty_test() ->
    [] = find_node( bogus, [] ).
find_node_mismatch_test() ->
    [] = find_node( bogus, [ { one, "one" } ] ).
find_node_match_test() ->
    "matchingPID" = find_node( bogus, [ { one, "one" }, { two, "two"}, { bogus, "matchingPID" }, { three, "three" } ] ).

lookup_test() ->
    start_registry(),
    TestNode = self(),  %"test node",
    registry ! { add, testNode, TestNode },
    % ?assert( TestNode == registry ! { lookup, testNode, self() } ),
    registry ! { lookup, testNode, self() },
    receive
        { [] } ->
            io:format( "~w failed~n", [ self() ] );
        { PID } ->
            io:format( "~w got response ~w~n", [ self(), PID ] ),
            PID = TestNode
    end.

list_test() ->
    start_registry(),
    registry ! { clear },
    registry ! { add, testNode, "test node" },
    registry ! { add, testNodeTwo, "test node two" },
    registry ! { list_nodes, self() },
    receive
        { [] } ->
            io:format( "~w failed~n", [ self() ] ),
            result = "fail";
        { Nodes } ->
            io:format( "~w got response:~n", [ self() ] ),
            [ io:format( "Node ~w: ~w~n", [ Name, PID ] ) || { Name, PID } <- Nodes ]
            % Nodes = [ { testNode, "test node" }, { testNodeTwo, "test node two" } ]
    end.

start_node( Id ) ->
    spawn( reg, init_node, [ Id ] ).
init_node( Id ) ->
    registry ! { add, Id, self() }.
node_loop() ->
    timer:sleep(5000),  % sleep for 5 seconds
    io:format( "Not implemented yet~n", [] ).

register_node( Id, Node ) ->
    io:format( "Not implemented yet~n", [] ).
update() ->
    io:format( "Not implemented yet~n", [] ).


