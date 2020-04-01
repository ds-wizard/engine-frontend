module String.Extra exposing (toMaybe)


toMaybe : String -> Maybe String
toMaybe str =
    if String.isEmpty str then
        Nothing

    else
        Just str
