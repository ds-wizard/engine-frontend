module Wizard.Api.Models.Event.EditQuestionItemSelectData exposing
    ( EditQuestionItemSelectEventData
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


type alias EditQuestionItemSelectEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhaseUuid : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , listQuestionUuid : EventField (Maybe String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditQuestionItemSelectEventData
decoder =
    D.succeed EditQuestionItemSelectEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhaseUuid" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "listQuestionUuid" (EventField.decoder (D.nullable D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditQuestionItemSelectEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ItemSelectQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhaseUuid", EventField.encode (E.maybe E.string) data.requiredPhaseUuid )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "listQuestionUuid", EventField.encode (E.maybe E.string) data.listQuestionUuid )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditQuestionItemSelectEventData
init =
    { title = EventField.empty
    , text = EventField.empty
    , requiredPhaseUuid = EventField.empty
    , tagUuids = EventField.empty
    , referenceUuids = EventField.empty
    , expertUuids = EventField.empty
    , listQuestionUuid = EventField.empty
    , annotations = EventField.empty
    }


squash : EditQuestionItemSelectEventData -> EditQuestionItemSelectEventData -> EditQuestionItemSelectEventData
squash left right =
    { title = EventField.squash left.title right.title
    , text = EventField.squash left.text right.text
    , requiredPhaseUuid = EventField.squash left.requiredPhaseUuid right.requiredPhaseUuid
    , tagUuids = EventField.squash left.tagUuids right.tagUuids
    , referenceUuids = EventField.squash left.referenceUuids right.referenceUuids
    , expertUuids = EventField.squash left.expertUuids right.expertUuids
    , listQuestionUuid = EventField.squash left.listQuestionUuid right.listQuestionUuid
    , annotations = EventField.squash left.annotations right.annotations
    }
