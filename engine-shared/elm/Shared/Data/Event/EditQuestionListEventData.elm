module Shared.Data.Event.EditQuestionListEventData exposing
    ( EditQuestionListEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditQuestionListEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredPhase : EventField (Maybe String)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , itemTemplateQuestionUuids : EventField (List String)
    }


decoder : Decoder EditQuestionListEventData
decoder =
    D.succeed EditQuestionListEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredPhase" (EventField.decoder (D.nullable D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "itemTemplateQuestionUuids" (EventField.decoder (D.list D.string))


encode : EditQuestionListEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ListQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredPhase", EventField.encode (E.maybe E.string) data.requiredPhase )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "itemTemplateQuestionUuids", EventField.encode (E.list E.string) data.itemTemplateQuestionUuids )
    ]
