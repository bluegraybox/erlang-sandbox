-module(fib).
-export([calc/1]).

calc( 0 ) -> [ 0 ];
calc( 1 ) -> [ 1 ];
calc( X ) ->
    L = calc( X - 1 ),
    [ Pre1 | T ] = L,
    [ Pre2 | T2 ] = calc( X - 2 ),
    Val = Pre1 + Pre2,
    [ Val | L ].

