module Math exposing (divModBy)


divModBy : Int -> Int -> ( Int, Int )
divModBy base x =
    ( x // base, modBy base x )
