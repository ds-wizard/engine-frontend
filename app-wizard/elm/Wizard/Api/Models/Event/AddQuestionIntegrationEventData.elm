module Wizard.Api.Models.Event.AddQuestionIntegrationEventData exposing
    ( AddQuestionIntegrationEventData
    , decoder
    , encode
    , toQuestion
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question(..))


type alias AddQuestionIntegrationEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , integrationUuid : String
    , variables : Dict String String
    , annotations : List Annotation
    }


decoder : Decoder AddQuestionIntegrationEventData
decoder =
    D.succeed AddQuestionIntegrationEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "integrationUuid" D.string
        |> D.required "variables" (D.dict D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddQuestionIntegrationEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "IntegrationQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "integrationUuid", E.string data.integrationUuid )
    , ( "variables", E.dict identity E.string data.variables )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


toQuestion : String -> AddQuestionIntegrationEventData -> Question
toQuestion uuid data =
    IntegrationQuestion
        { uuid = uuid
        , title = data.title
        , text = data.text
        , requiredPhaseUuid = data.requiredPhaseUuid
        , tagUuids = data.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , annotations = data.annotations
        }
        { integrationUuid = ""
        , variables = Dict.empty
        }
