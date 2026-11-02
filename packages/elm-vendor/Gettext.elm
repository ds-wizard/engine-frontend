module Gettext exposing
    ( Locale
    , defaultLocale
    , gettext
    , localeDecoder
    , ngettext
    )

import Dict exposing (Dict)
import Gettext.Plural as Plural
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List


type alias Locale =
    { messages : Dict String (List String)
    , plural : Plural.PluralAst
    }


defaultLocale : Locale
defaultLocale =
    { messages = Dict.empty
    , plural = Plural.defaultPluralAst
    }


{-| A translation as Jed JSON (`po2json -f jed`, or the server's own conversion
of the same PO). Whatever a translation leaves out or holds that this does not
use reads as no translation of that string, never as a translation that cannot
be read: a missing or unreadable `Plural-Forms` is the English rule, a `null`
form is an empty one (the msgid is shown), and a context key (`ctx\u{0004}msgid`)
is never asked for.
-}
localeDecoder : Decoder Locale
localeDecoder =
    let
        form =
            D.oneOf [ D.string, D.succeed "" ]

        messagesDecoder =
            D.dict
                (D.oneOf
                    [ D.list form
                    , D.succeed []
                    ]
                )

        pluralAst str =
            str
                |> (List.last << String.split "plural=")
                |> Maybe.andThen (List.head << String.split ";")
                |> Maybe.andThen (Result.toMaybe << Plural.fromString)
                |> Maybe.withDefault Plural.defaultPluralAst

        pluralDecoder =
            D.oneOf
                [ D.at [ "locale_data", "messages", "", "plural_forms" ] (D.map pluralAst D.string)
                , D.succeed Plural.defaultPluralAst
                ]
    in
    D.succeed Locale
        |> D.requiredAt [ "locale_data", "messages" ] messagesDecoder
        |> D.custom pluralDecoder


gettext : String -> Locale -> String
gettext msgid locale =
    case Maybe.andThen List.head (Dict.get msgid locale.messages) of
        Just value ->
            if String.isEmpty value then
                msgid

            else
                value

        Nothing ->
            msgid


ngettext : ( String, String ) -> Int -> Locale -> String
ngettext ( singular, plural ) n lang =
    let
        pluralIndex =
            Plural.eval lang.plural n

        default =
            if n == 1 then
                singular

            else
                plural
    in
    case Dict.get singular lang.messages of
        Just options ->
            case List.getAt pluralIndex options of
                Just message ->
                    if String.isEmpty message then
                        default

                    else
                        message

                Nothing ->
                    default

        Nothing ->
            default
