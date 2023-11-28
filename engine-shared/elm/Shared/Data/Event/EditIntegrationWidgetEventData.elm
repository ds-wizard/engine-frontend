module Shared.Data.Event.EditIntegrationWidgetEventData exposing
    ( EditIntegrationWidgetEventData
    , decoder
    , encode
    , init
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.Event.EventField as EventField exposing (EventField)
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias EditIntegrationWidgetEventData =
    { id : EventField String
    , name : EventField String
    , props : EventField (List String)
    , logo : EventField (Maybe String)
    , itemUrl : EventField (Maybe String)
    , annotations : EventField (List Annotation)
    , widgetUrl : EventField String
    }


decoder : Decoder EditIntegrationWidgetEventData
decoder =
    D.succeed EditIntegrationWidgetEventData
        |> D.required "id" (EventField.decoder D.string)
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "props" (EventField.decoder (D.list D.string))
        |> D.required "logo" (EventField.decoder (D.maybe D.string))
        |> D.required "itemUrl" (EventField.decoder (D.maybe D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))
        |> D.required "widgetUrl" (EventField.decoder D.string)


encode : EditIntegrationWidgetEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "WidgetIntegration" )
    , ( "id", EventField.encode E.string data.id )
    , ( "name", EventField.encode E.string data.name )
    , ( "props", EventField.encode (E.list E.string) data.props )
    , ( "logo", EventField.encode (E.maybe E.string) data.logo )
    , ( "itemUrl", EventField.encode (E.maybe E.string) data.itemUrl )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    , ( "widgetUrl", EventField.encode E.string data.widgetUrl )
    ]


init : EditIntegrationWidgetEventData
init =
    { id = EventField.empty
    , name = EventField.empty
    , props = EventField.empty
    , logo = EventField.empty
    , itemUrl = EventField.empty
    , annotations = EventField.empty
    , widgetUrl = EventField.empty
    }
