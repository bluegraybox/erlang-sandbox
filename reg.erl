-module(reg).

-include_lib("eunit/include/eunit.hrl").

% set_registry has to be exported, even though it's only called internally.  I don't understand...
-export([ start_registry/0, set_registry/1, start_node/1 ]).


start_registry() ->
    PID = whereis( registry ),
    if
        undefined == PID ->
            register( registry, spawn( reg, set_registry, [ dict:new() ] ) );
        true ->
            PID
    end.


% Can we make this a meta-server, so it could be configured with arbitrary message handlers?
set_registry( Nodes ) ->
    io:format("~w registry wait loop~n", [ self() ]),
    receive
        { add, Name, PID } ->
            io:format("~w adding node ~w: ~w~n", [ self(), Name, PID ]),
            set_registry( dict:store( Name, PID, Nodes ) );
        { lookup, Name, PID } ->
            io:format("~w got lookup request for node ~w from ~w~n", [ self(), Name, PID ]),
            {ok, Match} = dict:find( Name, Nodes ),
            PID ! { Match },
            set_registry( Nodes );
        { list_nodes, PID } ->
            NodeList = dict:to_list( Nodes ),
            [ io:format( "Node ~w: ~w~n", [ Name, Node_PID ] ) || { Name, Node_PID } <- NodeList ],
            PID ! { Nodes },
            set_registry( Nodes );
        { clear } ->
            io:format( "~w clearing registry~n", [ self() ] ),
            set_registry( dict:new() );
        { stop } ->
            io:format( "~w exiting~n", [ self() ] );
        Unknown ->
            dump_thing( Unknown ),
            set_registry( Nodes )
    end.


dump_thing( Thing ) ->
    if
        is_tuple( Thing ) ->
            io:format( "~w unknown message ~w~n", [ self(), lists:concat( tuple_to_list( Thing ) ) ] );
        is_list( Thing ) ->
            io:format( "~w unknown message ~w~n", [ self(), lists:concat( Thing ) ] );
        true ->
            io:format( "~w unknown message ~w~n", [ self(), Thing ] )
    end.


lookup_test() ->
    start_registry(),
    TestNode = self(),  %"test node",
    registry ! { add, testNode, TestNode },
    % ?assert( TestNode == registry ! { lookup, testNode, self() } ),
    registry ! { lookup, testNode, self() },
    % FIXME: use asserts; add failure test.
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
        { Nodes } ->
            io:format( "~w got response:~n", [ self() ] ),
            "test node" = dict:fetch( testNode, Nodes ),
            "test node two" = dict:fetch( testNodeTwo, Nodes )
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


