{-
   Conversion to Roman numbers inspired by https://gist.github.com/ondrejsevcik/b7dc021de6f7397aafd77fc15a566b25
-}


module Roman exposing (toRomanNumber)

import List.Extra as List


conversionTable : List ( Int, String )
conversionTable =
    [ ( 1000, "M" )
    , ( 900, "CM" )
    , ( 500, "D" )
    , ( 400, "CD" )
    , ( 100, "C" )
    , ( 90, "XC" )
    , ( 50, "L" )
    , ( 40, "XL" )
    , ( 10, "X" )
    , ( 9, "IX" )
    , ( 5, "V" )
    , ( 4, "IV" )
    , ( 1, "I" )
    ]


toRomanNumber : Int -> String
toRomanNumber arabicInput =
    if arabicInput <= 0 then
        ""

    else
        let
            ( arabic, roman ) =
                conversionTable
                    |> List.find (\( a, r ) -> a <= arabicInput)
                    |> Maybe.withDefault ( 0, "" )
        in
        roman ++ toRomanNumber (arabicInput - arabic)
