module Common.Utils.ByteUnits exposing (toReadable)

import Round


toReadable : Int -> String
toReadable num =
    let
        sign =
            if num < 0 then
                "-"

            else
                ""

        units =
            [ "B", "kB", "MB", "GB", "TB", "PB" ]

        toString value unit =
            String.fromFloat (Round.roundNum 2 value) ++ " " ++ unit

        fold unitsLeft value =
            case unitsLeft of
                head :: [] ->
                    toString value head

                head :: tail ->
                    if value < 1000 then
                        toString value head

                    else
                        fold tail (value / 1000)

                [] ->
                    ""
    in
    sign ++ fold units (toFloat (abs num))
