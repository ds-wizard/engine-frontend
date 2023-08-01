module CharIdentifier exposing (fromInt)

import Math exposing (divModBy)


fromInt : Int -> String
fromInt n =
    if n < 0 then
        ""

    else
        let
            ( quotient, remainder ) =
                divModBy 26 n
        in
        fromInt (quotient - 1) ++ String.fromChar (Char.fromCode (remainder + 97))
