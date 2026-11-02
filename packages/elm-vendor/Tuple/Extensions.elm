module Tuple.Extensions exposing
    ( T4(..)
    , prepend
    , sum
    )


type T4 a b c d
    = T4 a b c d


prepend : a -> ( b, c ) -> ( a, b, c )
prepend a ( b, c ) =
    ( a, b, c )


sum : ( number, number ) -> ( number, number ) -> ( number, number )
sum =
    map2 (+)


map2 : (a -> b -> value) -> ( a, a ) -> ( b, b ) -> ( value, value )
map2 fn ( a1, a2 ) ( b1, b2 ) =
    ( fn a1 b1, fn a2 b2 )
