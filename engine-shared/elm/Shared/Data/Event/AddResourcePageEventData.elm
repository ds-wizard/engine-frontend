module Shared.Data.Event.AddResourcePageEventData exposing
    ( AddResourcePageEventData
    , decoder
    , encode
    , init
    , toResourcePage
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.ResourcePage exposing (ResourcePage)


type alias AddResourcePageEventData =
    { title : String
    , content : String
    , annotations : List Annotation
    }


decoder : Decoder AddResourcePageEventData
decoder =
    D.succeed AddResourcePageEventData
        |> D.required "title" D.string
        |> D.required "content" D.string
        |> D.required "annotations" (D.list Annotation.decoder)


encode : AddResourcePageEventData -> List ( String, E.Value )
encode eventData =
    [ ( "eventType", E.string "AddResourcePageEvent" )
    , ( "title", E.string eventData.title )
    , ( "content", E.string eventData.content )
    , ( "annotations", E.list Annotation.encode eventData.annotations )
    ]


init : AddResourcePageEventData
init =
    { title = ""
    , content = ""
    , annotations = []
    }


toResourcePage : String -> AddResourcePageEventData -> ResourcePage
toResourcePage resourcePageUuid data =
    { uuid = resourcePageUuid
    , title = data.title
    , content = data.content
    , annotations = data.annotations
    }
