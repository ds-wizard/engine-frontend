module KMEditor.Common.KnowledgeModel.Question.IntegrationQuestionData exposing
    ( IntegrationQuestionData
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias IntegrationQuestionData =
    { integrationUuid : String
    , props : Dict String String
    }


decoder : Decoder IntegrationQuestionData
decoder =
    D.succeed IntegrationQuestionData
        |> D.required "integrationUuid" D.string
        |> D.required "props" (D.dict D.string)
