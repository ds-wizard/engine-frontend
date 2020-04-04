module String.Extra exposing (fromMaybe, toMaybe)


toMaybe : String -> Maybe String
toMaybe str =
    if String.isEmpty str then
        Nothing

    else
        Just str


fromMaybe : Maybe String -> String
fromMaybe =
    Maybe.withDefault ""
