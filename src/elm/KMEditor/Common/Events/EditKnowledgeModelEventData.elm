module KMEditor.Common.Events.EditKnowledgeModelEventData exposing
    ( EditKnowledgeModelEventData
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import KMEditor.Common.Events.EventField as EventField exposing (EventField)


type alias EditKnowledgeModelEventData =
    { name : EventField String
    , chapterUuids : EventField (List String)
    , tagUuids : EventField (List String)
    , integrationUuids : EventField (List String)
    }


decoder : Decoder EditKnowledgeModelEventData
decoder =
    D.succeed EditKnowledgeModelEventData
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "chapterUuids" (EventField.decoder (D.list D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "integrationUuids" (EventField.decoder (D.list D.string))


encode : EditKnowledgeModelEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditKnowledgeModelEvent" )
    , ( "name", EventField.encode E.string data.name )
    , ( "chapterUuids", EventField.encode (E.list E.string) data.chapterUuids )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "integrationUuids", EventField.encode (E.list E.string) data.integrationUuids )
    ]
