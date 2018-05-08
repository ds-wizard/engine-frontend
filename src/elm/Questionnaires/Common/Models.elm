module Questionnaires.Common.Models exposing (..)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import KnowledgeModels.Editor.Models.Entities exposing (KnowledgeModel, knowledgeModelDecoder)
import PackageManagement.Models exposing (PackageDetail, packageDetailDecoder)


type alias Questionnaire =
    { uuid : String
    , name : String
    , package : PackageDetail
    }


type alias QuestionnaireDetail =
    { uuid : String
    , name : String
    , package : PackageDetail
    , knowledgeModel : KnowledgeModel
    , values : Dict String String
    }


questionnaireDecoder : Decoder Questionnaire
questionnaireDecoder =
    decode Questionnaire
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" packageDetailDecoder


questionnaireListDecoder : Decoder (List Questionnaire)
questionnaireListDecoder =
    Decode.list questionnaireDecoder


questionnaireDetailDecoder : Decoder QuestionnaireDetail
questionnaireDetailDecoder =
    decode QuestionnaireDetail
        |> required "uuid" Decode.string
        |> required "name" Decode.string
        |> required "package" packageDetailDecoder
        |> required "knowledgeModel" knowledgeModelDecoder
        |> required "values" (Decode.dict Decode.string)
