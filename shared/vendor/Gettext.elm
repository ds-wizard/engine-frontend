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


localeDecoder : Decoder Locale
localeDecoder =
    let
        messagesDecoder =
            D.dict
                (D.oneOf
                    [ D.list D.string
                    , D.succeed []
                    ]
                )

        pluralDecoder =
            D.string
                |> D.andThen
                    (\str ->
                        let
                            mbPluralString =
                                str
                                    |> (List.last << String.split "plural=")
                                    |> Maybe.andThen (List.head << String.split ";")
                        in
                        case mbPluralString of
                            Just pluralString ->
                                case Plural.fromString pluralString of
                                    Ok pluralAst ->
                                        D.succeed pluralAst

                                    Err error ->
                                        D.fail error

                            Nothing ->
                                D.fail "Invalid plural string"
                    )
    in
    D.succeed Locale
        |> D.requiredAt [ "locale_data", "messages" ] messagesDecoder
        |> D.requiredAt [ "locale_data", "messages", "", "plural_forms" ] pluralDecoder


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
