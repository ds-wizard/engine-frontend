module Shared.Data.Event.AddIntegrationWidgetEventData exposing
    ( AddIntegrationWidgetEventData
    , decoder
    , encode
    , toIntegration
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Data.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Integration exposing (Integration(..))


type alias AddIntegrationWidgetEventData =
    { id : String
    , name : String
    , props : List String
    , logo : Maybe String
    , itemUrl : Maybe String
    , annotations : List Annotation
    , widgetUrl : String
    }


decoder : Decoder AddIntegrationWidgetEventData
decoder =
    D.succeed AddIntegrationWidgetEventData
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "props" (D.list D.string)
        |> D.required "logo" (D.maybe D.string)
        |> D.required "itemUrl" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "widgetUrl" D.string


encode : AddIntegrationWidgetEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "WidgetIntegration" )
    , ( "id", E.string data.id )
    , ( "name", E.string data.name )
    , ( "props", E.list E.string data.props )
    , ( "logo", E.maybe E.string data.logo )
    , ( "itemUrl", E.maybe E.string data.itemUrl )
    , ( "annotations", E.list Annotation.encode data.annotations )
    , ( "widgetUrl", E.string data.widgetUrl )
    ]


toIntegration : String -> AddIntegrationWidgetEventData -> Integration
toIntegration uuid data =
    WidgetIntegration
        { uuid = uuid
        , id = data.id
        , name = data.name
        , props = data.props
        , logo = data.logo
        , itemUrl = data.itemUrl
        , annotations = data.annotations
        }
        { widgetUrl = data.widgetUrl
        }
