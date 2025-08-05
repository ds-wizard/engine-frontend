module Wizard.Api.Models.QuestionnaireInfo exposing
    ( QuestionnaireInfo
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias QuestionnaireInfo =
    { uuid : Uuid
    , name : String
    }


decoder : Decoder QuestionnaireInfo
decoder =
    D.succeed QuestionnaireInfo
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
