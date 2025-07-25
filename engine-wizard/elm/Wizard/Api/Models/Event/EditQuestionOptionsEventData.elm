module Wizard.Api.Models.Event.EditQuestionOptionsEventData exposing
    ( EditQuestionOptionsEventData
    , decoder
    , encode
    , init
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.Event.EventField as EventField exposing (EventField)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditQuestionOptionsEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhaseUuid : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , answerUuids : EventField (List String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditQuestionOptionsEventData
decoder =
    D.succeed EditQuestionOptionsEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhaseUuid" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "answerUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditQuestionOptionsEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "OptionsQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhaseUuid", EventField.encode (E.maybe E.string) data.requiredPhaseUuid )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "answerUuids", EventField.encode (E.list E.string) data.answerUuids )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditQuestionOptionsEventData
init =
    { title = EventField.empty
    , text = EventField.empty
    , requiredPhaseUuid = EventField.empty
    , tagUuids = EventField.empty
    , referenceUuids = EventField.empty
    , expertUuids = EventField.empty
    , answerUuids = EventField.empty
    , annotations = EventField.empty
    }


squash : EditQuestionOptionsEventData -> EditQuestionOptionsEventData -> EditQuestionOptionsEventData
squash oldData newData =
    { title = EventField.squash oldData.title newData.title
    , text = EventField.squash oldData.text newData.text
    , requiredPhaseUuid = EventField.squash oldData.requiredPhaseUuid newData.requiredPhaseUuid
    , tagUuids = EventField.squash oldData.tagUuids newData.tagUuids
    , referenceUuids = EventField.squash oldData.referenceUuids newData.referenceUuids
    , expertUuids = EventField.squash oldData.expertUuids newData.expertUuids
    , answerUuids = EventField.squash oldData.answerUuids newData.answerUuids
    , annotations = EventField.squash oldData.annotations newData.annotations
    }
