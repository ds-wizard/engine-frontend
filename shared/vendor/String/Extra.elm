module String.Extra exposing
    ( fromBool
    , fromMaybe
    , insertAt
    , removeNonNumbers
    , stripQuotes
    , toBool
    , toMaybe
    , withDefault
    )

import Regex


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


toBool : String -> Maybe Bool
toBool str =
    if str == "true" then
        Just True

    else if str == "false" then
        Just False

    else
        Nothing


removeNonNumbers : String -> String
removeNonNumbers inputString =
    case Regex.fromString "[^0-9]" of
        Nothing ->
            inputString

        Just regex ->
            Regex.replace regex (\_ -> "") inputString


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


insertAt : Int -> String -> String -> String
insertAt pos insert original =
    let
        safePos =
            clamp 0 (String.length original) pos
    in
    String.left safePos original
        ++ insert
        ++ String.dropLeft safePos original
