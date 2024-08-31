module Shared.Data.Event.EditResourceCollectionEventData exposing
    ( EditResourceCollectionEventData
    , apply
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.ResourceCollection exposing (ResourceCollection)


type alias EditResourceCollectionEventData =
    { title : EventField String
    , resourcePageUuids : EventField (List String)
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditResourceCollectionEventData
decoder =
    D.succeed EditResourceCollectionEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "resourcePageUuids" (EventField.decoder (D.list D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditResourceCollectionEventData -> List ( String, E.Value )
encode eventData =
    [ ( "eventType", E.string "EditResourceCollectionEvent" )
    , ( "title", EventField.encode E.string eventData.title )
    , ( "resourcePageUuids", EventField.encode (E.list E.string) eventData.resourcePageUuids )
    , ( "annotations", EventField.encode (E.list Annotation.encode) eventData.annotations )
    ]


init : EditResourceCollectionEventData
init =
    { title = EventField.empty
    , resourcePageUuids = EventField.empty
    , annotations = EventField.empty
    }


apply : EditResourceCollectionEventData -> ResourceCollection -> ResourceCollection
apply eventData resourceCollection =
    { resourceCollection
        | title = EventField.getValueWithDefault eventData.title resourceCollection.title
        , resourcePageUuids = EventField.applyChildren eventData.resourcePageUuids resourceCollection.resourcePageUuids
        , annotations = EventField.getValueWithDefault eventData.annotations resourceCollection.annotations
    }