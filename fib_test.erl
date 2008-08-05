-module(fib_test).
-include_lib("eunit/include/eunit.hrl").

fib_zero_test()  -> [ 0 ]             = fib:calc( 0 ).
fib_one_test()   -> [ 1, 0 ]          = fib:calc( 1 ).
fib_two_test()   -> [ 1, 1, 0 ]       = fib:calc( 2 ).
fib_three_test() -> [ 2, 1, 1, 0 ]    = fib:calc( 3 ).
fib_four_test()  -> [ 3, 2, 1, 1, 0 ] = fib:calc( 4 ).

