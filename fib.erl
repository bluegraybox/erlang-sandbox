-module(fib).
-export([calc/1]).

calc( 0 ) -> [ 0 ];
calc( 1 ) -> [ 1, 0 ];
calc( X ) ->
    [ Pre1, Pre2 | T1 ] = calc( X - 1 ),
    [ Pre1 + Pre2 | [ Pre1 | [ Pre2 | T1 ] ] ].

