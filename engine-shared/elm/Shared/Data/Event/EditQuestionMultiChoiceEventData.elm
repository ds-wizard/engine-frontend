module Shared.Data.Event.EditQuestionMultiChoiceEventData exposing (EditQuestionMultiChoiceEventData, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditQuestionMultiChoiceEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhase : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , choiceUuids : EventField (List String)
    }


decoder : Decoder EditQuestionMultiChoiceEventData
decoder =
    D.succeed EditQuestionMultiChoiceEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhase" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "choiceUuids" (EventField.decoder (D.list D.string))


encode : EditQuestionMultiChoiceEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "MultiChoiceQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhase", EventField.encode (E.maybe E.string) data.requiredPhase )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "choiceUuids", EventField.encode (E.list E.string) data.choiceUuids )
    ]
