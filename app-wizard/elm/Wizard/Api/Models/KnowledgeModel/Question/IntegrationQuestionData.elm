module Wizard.Api.Models.KnowledgeModel.Question.IntegrationQuestionData exposing
    ( IntegrationQuestionData
    , decoder
    , encodeValues
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias IntegrationQuestionData =
    { integrationUuid : String
    , variables : Dict String String
    }


decoder : Decoder IntegrationQuestionData
decoder =
    D.succeed IntegrationQuestionData
        |> D.required "integrationUuid" D.string
        |> D.required "variables" (D.dict D.string)


encodeValues : IntegrationQuestionData -> List ( String, E.Value )
encodeValues integrationData =
    [ ( "integrationUuid", E.string integrationData.integrationUuid )
    , ( "variables", E.dict identity E.string integrationData.variables )
    ]
