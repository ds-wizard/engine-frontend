module String.Extra exposing (fromMaybe, toMaybe, withDefault)


toMaybe : String -> Maybe String
toMaybe str =
    if String.isEmpty str then
        Nothing

    else
        Just str


fromMaybe : Maybe String -> String
fromMaybe =
    Maybe.withDefault ""


withDefault : String -> String -> String
withDefault default string =
    if String.isEmpty string then
        default

    else
        string
