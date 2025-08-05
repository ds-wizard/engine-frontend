module Tuple.Extra exposing (mergeSame, sum)


sum : ( number, number ) -> ( number, number ) -> ( number, number )
sum =
    mergeSame (+)


mergeSame : (a -> a -> b) -> ( a, a ) -> ( a, a ) -> ( b, b )
mergeSame fn ( a1, a2 ) ( b1, b2 ) =
    ( fn a1 b1, fn a2 b2 )
