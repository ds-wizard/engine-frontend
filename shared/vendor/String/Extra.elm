module String.Extra exposing (fromBool, fromMaybe, stripQuotes, toMaybe, withDefault)


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


fromBool : Bool -> String
fromBool bool =
    if bool then
        "true"

    else
        "false"


stripQuotes : String -> String
stripQuotes str =
    str
        |> String.trim
        |> (\s ->
                if String.startsWith "\"" s && String.endsWith "\"" s then
                    String.dropRight 1 (String.dropLeft 1 s)

                else if String.startsWith "'" s && String.endsWith "'" s then
                    String.dropRight 1 (String.dropLeft 1 s)

                else
                    s
           )
