module Shared.Data.Event.EditQuestionMultiChoiceEventData exposing (EditQuestionMultiChoiceEventData, decoder, encode, init)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditQuestionMultiChoiceEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhaseUuid : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , choiceUuids : EventField (List String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditQuestionMultiChoiceEventData
decoder =
    D.succeed EditQuestionMultiChoiceEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhaseUuid" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "choiceUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditQuestionMultiChoiceEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "MultiChoiceQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhaseUuid", EventField.encode (E.maybe E.string) data.requiredPhaseUuid )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "choiceUuids", EventField.encode (E.list E.string) data.choiceUuids )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    ]


init : EditQuestionMultiChoiceEventData
init =
    { title = EventField.empty
    , text = EventField.empty
    , requiredPhaseUuid = EventField.empty
    , tagUuids = EventField.empty
    , referenceUuids = EventField.empty
    , expertUuids = EventField.empty
    , choiceUuids = EventField.empty
    , annotations = EventField.empty
    }
