module Questionnaires.Common.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias Questionnaire =
    { uuid : String
    , pkgId : String
    , name : String
    }


questionnaireDecoder : Decoder Questionnaire
questionnaireDecoder =
    decode Questionnaire
        |> required "uuid" Decode.string
        |> required "pkgId" Decode.string
        |> required "name" Decode.string


questionnaireListDecoder : Decoder (List Questionnaire)
questionnaireListDecoder =
    Decode.list questionnaireDecoder
