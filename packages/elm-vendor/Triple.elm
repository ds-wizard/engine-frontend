module Triple exposing (mapSnd, second)


mapSnd : (b -> d) -> ( a, b, c ) -> ( a, d, c )
mapSnd fn ( a, b, c ) =
    ( a, fn b, c )


second : ( a, b, c ) -> b
second ( _, b, _ ) =
    b
