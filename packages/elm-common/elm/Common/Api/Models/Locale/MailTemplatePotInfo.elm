module Common.Api.Models.Locale.MailTemplatePotInfo exposing
    ( MailTemplatePotInfo
    , PotSource(..)
    , decoder
    )

{-| Which templates the mails use — the default ones or the organization's own
— and the version of their POT; none while there is no POT yet.
-}

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias MailTemplatePotInfo =
    { source : PotSource
    , potVersion : Maybe String
    }


type PotSource
    = DefaultPot
    | CustomPot
    | NoPot


decoder : Decoder MailTemplatePotInfo
decoder =
    D.succeed MailTemplatePotInfo
        |> D.required "source" sourceDecoder
        |> D.optional "potVersion" (D.nullable D.string) Nothing


sourceDecoder : Decoder PotSource
sourceDecoder =
    D.string
        |> D.map
            (\source ->
                case source of
                    "default" ->
                        DefaultPot

                    "custom" ->
                        CustomPot

                    _ ->
                        NoPot
            )
