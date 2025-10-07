module Common.Utils.Bool exposing
    ( toInt
    , toString
    )


toInt : Bool -> Int
toInt bool =
    if bool then
        1

    else
        0


toString : Bool -> String
toString bool =
    if bool then
        "true"

    else
        "false"
