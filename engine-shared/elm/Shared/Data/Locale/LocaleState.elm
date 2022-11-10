module Shared.Data.Locale.LocaleState exposing
    ( LocaleState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type LocaleState
    = Unknown
    | Outdated
    | UpToDate
    | Unpublished


decoder : Decoder LocaleState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "UnknownLocaleState" ->
                        D.succeed Unknown

                    "OutdatedLocaleState" ->
                        D.succeed Outdated

                    "UpToDateLocaleState" ->
                        D.succeed UpToDate

                    "UnpublishedLocaleState" ->
                        D.succeed Unpublished

                    _ ->
                        D.fail <| "Unknown locale state: " ++ str
            )
