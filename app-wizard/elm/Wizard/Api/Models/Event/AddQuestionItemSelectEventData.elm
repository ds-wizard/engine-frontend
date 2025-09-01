module Wizard.Api.Models.Event.AddQuestionItemSelectEventData exposing
    ( AddQuestionItemSelectEventData
    , decoder
    , encode
    , toQuestion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question(..))


type alias AddQuestionItemSelectEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , listQuestionUuid : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder AddQuestionItemSelectEventData
decoder =
    D.succeed AddQuestionItemSelectEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "listQuestionUuid" (D.nullable D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddQuestionItemSelectEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "IntegrationQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "listQuestionUuid", E.maybe E.string data.listQuestionUuid )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


toQuestion : String -> AddQuestionItemSelectEventData -> Question
toQuestion uuid data =
    ItemSelectQuestion
        { uuid = uuid
        , title = data.title
        , text = data.text
        , requiredPhaseUuid = data.requiredPhaseUuid
        , tagUuids = data.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , annotations = data.annotations
        }
        { listQuestionUuid = Nothing
        }
