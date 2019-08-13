module KMEditor.Common.Events.EditQuestionListEventData exposing
    ( EditQuestionListEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import KMEditor.Common.Events.EventField as EventField exposing (EventField)


type alias EditQuestionListEventData =
    { title : EventField String
    , text : EventField (Maybe String)
    , requiredLevel : EventField (Maybe Int)
    , tagUuids : EventField (List String)
    , referenceUuids : EventField (List String)
    , expertUuids : EventField (List String)
    , itemTemplateTitle : EventField String
    , itemTemplateQuestionUuids : EventField (List String)
    }


decoder : Decoder EditQuestionListEventData
decoder =
    D.succeed EditQuestionListEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "text" (EventField.decoder (D.nullable D.string))
        |> D.required "requiredLevel" (EventField.decoder (D.nullable D.int))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "referenceUuids" (EventField.decoder (D.list D.string))
        |> D.required "expertUuids" (EventField.decoder (D.list D.string))
        |> D.required "itemTemplateTitle" (EventField.decoder D.string)
        |> D.required "itemTemplateQuestionUuids" (EventField.decoder (D.list D.string))


encode : EditQuestionListEventData -> List ( String, E.Value )
encode data =
    [ ( "questionType", E.string "ListQuestion" )
    , ( "title", EventField.encode E.string data.title )
    , ( "text", EventField.encode (E.maybe E.string) data.text )
    , ( "requiredLevel", EventField.encode (E.maybe E.int) data.requiredLevel )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "referenceUuids", EventField.encode (E.list E.string) data.referenceUuids )
    , ( "expertUuids", EventField.encode (E.list E.string) data.expertUuids )
    , ( "itemTemplateTitle", EventField.encode E.string data.itemTemplateTitle )
    , ( "itemTemplateQuestionUuids", EventField.encode (E.list E.string) data.itemTemplateQuestionUuids )
    ]
