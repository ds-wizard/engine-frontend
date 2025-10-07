module Wizard.Api.Models.Event.EditQuestionFileEventData exposing
    ( EditQuestionFileEventData
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


type alias EditQuestionFileEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhaseUuid : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , maxSize : EventField (Maybe Int)
    , fileTypes : EventField (Maybe String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditQuestionFileEventData
decoder =
    D.succeed EditQuestionFileEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhaseUuid" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "maxSize" (EventField.decoder (D.maybe D.int))
        |> D.required "fileTypes" (EventField.decoder (D.maybe D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditQuestionFileEventData -> List ( String, D.Value )
encode data =
    [ ( "questionType", E.string "FileQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhaseUuid", EventField.encode (E.maybe E.string) data.requiredPhaseUuid )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "maxSize", EventField.encode (E.maybe E.int) data.maxSize )
    , ( "fileTypes", EventField.encode (E.maybe E.string) data.fileTypes )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditQuestionFileEventData
init =
    { title = EventField.empty
    , text = EventField.empty
    , requiredPhaseUuid = EventField.empty
    , tagUuids = EventField.empty
    , referenceUuids = EventField.empty
    , expertUuids = EventField.empty
    , maxSize = EventField.empty
    , fileTypes = EventField.empty
    , annotations = EventField.empty
    }


squash : EditQuestionFileEventData -> EditQuestionFileEventData -> EditQuestionFileEventData
squash left right =
    { title = EventField.squash left.title right.title
    , text = EventField.squash left.text right.text
    , requiredPhaseUuid = EventField.squash left.requiredPhaseUuid right.requiredPhaseUuid
    , tagUuids = EventField.squash left.tagUuids right.tagUuids
    , referenceUuids = EventField.squash left.referenceUuids right.referenceUuids
    , expertUuids = EventField.squash left.expertUuids right.expertUuids
    , maxSize = EventField.squash left.maxSize right.maxSize
    , fileTypes = EventField.squash left.fileTypes right.fileTypes
    , annotations = EventField.squash left.annotations right.annotations
    }
