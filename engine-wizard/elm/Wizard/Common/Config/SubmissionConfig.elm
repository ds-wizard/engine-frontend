module Wizard.Common.Config.SubmissionConfig exposing
    ( SubmissionConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias SubmissionConfig =
    { enabled : Bool }


decoder : Decoder SubmissionConfig
decoder =
    D.succeed SubmissionConfig
        |> D.required "enabled" D.bool


default : SubmissionConfig
default =
    { enabled = False }
