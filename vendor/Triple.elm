module Triple exposing (mapSnd)


mapSnd : (b -> d) -> ( a, b, c ) -> ( a, d, c )
mapSnd fn ( a, b, c ) =
    ( a, fn b, c )
