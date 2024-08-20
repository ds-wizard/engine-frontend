module Shared.Data.Event.EditResourcePageEventData exposing
    ( EditResourcePageEventData
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
import Shared.Data.KnowledgeModel.ResourcePage exposing (ResourcePage)


type alias EditResourcePageEventData =
    { title : EventField String
    , content : EventField String
    , annotations : EventField (List Annotation)
    }


decoder : Decoder EditResourcePageEventData
decoder =
    D.succeed EditResourcePageEventData
        |> D.required "title" (EventField.decoder D.string)
        |> D.required "content" (EventField.decoder D.string)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))


encode : EditResourcePageEventData -> List ( String, E.Value )
encode eventData =
    [ ( "eventType", E.string "EditResourcePageEvent" )
    , ( "title", EventField.encode E.string eventData.title )
    , ( "content", EventField.encode E.string eventData.content )
    , ( "annotations", EventField.encode (E.list Annotation.encode) eventData.annotations )
    ]


init : EditResourcePageEventData
init =
    { title = EventField.empty
    , content = EventField.empty
    , annotations = EventField.empty
    }


apply : EditResourcePageEventData -> ResourcePage -> ResourcePage
apply eventData resourcePage =
    { resourcePage
        | title = EventField.getValueWithDefault eventData.title resourcePage.title
        , content = EventField.getValueWithDefault eventData.content resourcePage.content
        , annotations = EventField.getValueWithDefault eventData.annotations resourcePage.annotations
    }
