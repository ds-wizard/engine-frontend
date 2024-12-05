module Shared.Data.Event.EditQuestionValueEventData exposing
    ( EditQuestionValueEventData
    , decoder
    , encode
    , init
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Question.QuestionValidation as QuestionValidation exposing (QuestionValidation)
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType)


type alias EditQuestionValueEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhaseUuid : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , valueType : EventField QuestionValueType
    , validations : EventField (List QuestionValidation)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditQuestionValueEventData
decoder =
    D.succeed EditQuestionValueEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhaseUuid" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "valueType" (EventField.decoder QuestionValueType.decoder)
        |> D.required "validations" (EventField.decoder (D.list QuestionValidation.decoder))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditQuestionValueEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ValueQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhaseUuid", EventField.encode (E.maybe E.string) data.requiredPhaseUuid )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "valueType", EventField.encode QuestionValueType.encode data.valueType )
    , ( "validations", EventField.encode (E.list QuestionValidation.encode) data.validations )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditQuestionValueEventData
init =
    { title = EventField.empty
    , text = EventField.empty
    , requiredPhaseUuid = EventField.empty
    , tagUuids = EventField.empty
    , referenceUuids = EventField.empty
    , expertUuids = EventField.empty
    , valueType = EventField.empty
    , validations = EventField.empty
    , annotations = EventField.empty
    }


squash : EditQuestionValueEventData -> EditQuestionValueEventData -> EditQuestionValueEventData
squash oldData newData =
    { title = EventField.squash oldData.title newData.title
    , text = EventField.squash oldData.text newData.text
    , requiredPhaseUuid = EventField.squash oldData.requiredPhaseUuid newData.requiredPhaseUuid
    , tagUuids = EventField.squash oldData.tagUuids newData.tagUuids
    , referenceUuids = EventField.squash oldData.referenceUuids newData.referenceUuids
    , expertUuids = EventField.squash oldData.expertUuids newData.expertUuids
    , valueType = EventField.squash oldData.valueType newData.valueType
    , validations = EventField.squash oldData.validations newData.validations
    , annotations = EventField.squash oldData.annotations newData.annotations
    }
