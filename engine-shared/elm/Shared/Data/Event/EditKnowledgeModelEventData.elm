module Shared.Data.Event.EditKnowledgeModelEventData exposing
    ( EditKnowledgeModelEventData
    , decoder
    , encode
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)


type alias EditKnowledgeModelEventData =
    { chapterUuids : EventField (List String)
    , metricUuids : EventField (List String)
    , phaseUuids : EventField (List String)
    , tagUuids : EventField (List String)
    , integrationUuids : EventField (List String)
    , annotations : EventField (Dict String String)
    }


decoder : Decoder EditKnowledgeModelEventData
decoder =
    D.succeed EditKnowledgeModelEventData
        |> D.required "chapterUuids" (EventField.decoder (D.list D.string))
        |> D.required "metricUuids" (EventField.decoder (D.list D.string))
        |> D.required "phaseUuids" (EventField.decoder (D.list D.string))
        |> D.required "tagUuids" (EventField.decoder (D.list D.string))
        |> D.required "integrationUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.dict D.string))


encode : EditKnowledgeModelEventData -> List ( String, E.Value )
encode data =
    [ ( "eventType", E.string "EditKnowledgeModelEvent" )
    , ( "chapterUuids", EventField.encode (E.list E.string) data.chapterUuids )
    , ( "metricUuids", EventField.encode (E.list E.string) data.metricUuids )
    , ( "phaseUuids", EventField.encode (E.list E.string) data.phaseUuids )
    , ( "tagUuids", EventField.encode (E.list E.string) data.tagUuids )
    , ( "integrationUuids", EventField.encode (E.list E.string) data.integrationUuids )
    , ( "annotations", EventField.encode (E.dict identity E.string) data.annotations )
    ]
