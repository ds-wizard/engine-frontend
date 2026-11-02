module Common.Api.Models.Event.AddResourceCollectionEventData exposing
    ( AddResourceCollectionEventData
    , decoder
    , encode
    , init
    , toResourceCollection
    )

import Common.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Common.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias AddResourceCollectionEventData =
    { title : String
    , annotations : List Annotation
    }


decoder : Decoder AddResourceCollectionEventData
decoder =
    D.succeed AddResourceCollectionEventData
        |> D.required "title" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddResourceCollectionEventData -> List ( String, E.Value )
encode eventData =
    [ ( "eventType", E.string "AddResourceCollectionEvent" )
    , ( "title", E.string eventData.title )
    , ( "annotations", E.list Annotation.encode eventData.annotations )
    ]


init : AddResourceCollectionEventData
init =
    { title = ""
    , annotations = []
    }


toResourceCollection : String -> AddResourceCollectionEventData -> ResourceCollection
toResourceCollection resourceCollectionUuid data =
    { uuid = resourceCollectionUuid
    , title = data.title
    , resourcePageUuids = []
    , annotations = data.annotations
    }
