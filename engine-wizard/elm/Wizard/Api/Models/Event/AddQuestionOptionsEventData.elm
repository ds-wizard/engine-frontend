module Wizard.Api.Models.Event.AddQuestionOptionsEventData exposing
    ( AddQuestionOptionsEventData
    , decoder
    , encode
    , init
    , toQuestion
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Question exposing (Question(..))


type alias AddQuestionOptionsEventData =
    { title : String
    , text : Maybe String
    , requiredPhaseUuid : Maybe String
    , tagUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder AddQuestionOptionsEventData
decoder =
    D.succeed AddQuestionOptionsEventData
        |> D.required "title" D.string
        |> D.required "text" (D.nullable D.string)
        |> D.required "requiredPhaseUuid" (D.nullable D.string)
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddQuestionOptionsEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "OptionsQuestion" )
    , ( "title", E.string data.title )
    , ( "text", E.maybe E.string data.text )
    , ( "requiredPhaseUuid", E.maybe E.string data.requiredPhaseUuid )
    , ( "tagUuids", E.list E.string data.tagUuids )
    , ( "annotations", E.list Annotation.encode data.annotations )
    ]


init : AddQuestionOptionsEventData
init =
    { title = ""
    , text = Nothing
    , requiredPhaseUuid = Nothing
    , tagUuids = []
    , annotations = []
    }


toQuestion : String -> AddQuestionOptionsEventData -> Question
toQuestion uuid data =
    OptionsQuestion
        { uuid = uuid
        , title = data.title
        , text = data.text
        , requiredPhaseUuid = data.requiredPhaseUuid
        , tagUuids = data.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , annotations = data.annotations
        }
        { answerUuids = []
        }
