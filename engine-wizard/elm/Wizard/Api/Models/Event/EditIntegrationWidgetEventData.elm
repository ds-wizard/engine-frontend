module Wizard.Api.Models.Event.EditIntegrationWidgetEventData exposing
    ( EditIntegrationWidgetEventData
    , decoder
    , encode
    , init
    , squash
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.Event.EventField as EventField exposing (EventField)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


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


squash : EditIntegrationWidgetEventData -> EditIntegrationWidgetEventData -> EditIntegrationWidgetEventData
squash oldData newData =
    { id = EventField.squash oldData.id newData.id
    , name = EventField.squash oldData.name newData.name
    , props = EventField.squash oldData.props newData.props
    , logo = EventField.squash oldData.logo newData.logo
    , itemUrl = EventField.squash oldData.itemUrl newData.itemUrl
    , annotations = EventField.squash oldData.annotations newData.annotations
    , widgetUrl = EventField.squash oldData.widgetUrl newData.widgetUrl
    }
