module Wizard.Documents.Common.Submission exposing (Submission, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias Submission =
    { location : Maybe String }


decoder : Decoder Submission
decoder =
    D.succeed Submission
        |> D.required "location" (D.maybe D.string)
