module Wizard.Api.Models.Event.EditIntegrationApiEventData exposing
    ( EditIntegrationApiEventData
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
import Wizard.Api.Models.KnowledgeModel.Integration.RequestHeader as RequestHeader exposing (RequestHeader)


type alias EditIntegrationApiEventData =
    { id : EventField String
    , name : EventField String
    , props : EventField (List String)
    , logo : EventField (Maybe String)
    , itemUrl : EventField (Maybe String)
    , annotations : EventField (List Annotation)
    , requestMethod : EventField String
    , requestUrl : EventField String
    , requestHeaders : EventField (List RequestHeader)
    , requestBody : EventField String
    , requestEmptySearch : EventField Bool
    , responseListField : EventField (Maybe String)
    , responseItemId : EventField (Maybe String)
    , responseItemTemplate : EventField String
    }


decoder : Decoder EditIntegrationApiEventData
decoder =
    D.succeed EditIntegrationApiEventData
        |> D.required "id" (EventField.decoder D.string)
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "props" (EventField.decoder (D.list D.string))
        |> D.required "logo" (EventField.decoder (D.maybe D.string))
        |> D.required "itemUrl" (EventField.decoder (D.maybe D.string))
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))
        |> D.required "requestMethod" (EventField.decoder D.string)
        |> D.required "requestUrl" (EventField.decoder D.string)
        |> D.required "requestHeaders" (EventField.decoder (D.list RequestHeader.decoder))
        |> D.required "requestBody" (EventField.decoder D.string)
        |> D.required "requestEmptySearch" (EventField.decoder D.bool)
        |> D.required "responseListField" (EventField.decoder (D.maybe D.string))
        |> D.required "responseItemId" (EventField.decoder (D.maybe D.string))
        |> D.required "responseItemTemplate" (EventField.decoder D.string)


encode : EditIntegrationApiEventData -> List ( String, E.Value )
encode data =
    [ ( "integrationType", E.string "ApiIntegration" )
    , ( "id", EventField.encode E.string data.id )
    , ( "name", EventField.encode E.string data.name )
    , ( "props", EventField.encode (E.list E.string) data.props )
    , ( "logo", EventField.encode (E.maybe E.string) data.logo )
    , ( "itemUrl", EventField.encode (E.maybe E.string) data.itemUrl )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    , ( "requestMethod", EventField.encode E.string data.requestMethod )
    , ( "requestUrl", EventField.encode E.string data.requestUrl )
    , ( "requestHeaders", EventField.encode (E.list RequestHeader.encode) data.requestHeaders )
    , ( "requestBody", EventField.encode E.string data.requestBody )
    , ( "requestEmptySearch", EventField.encode E.bool data.requestEmptySearch )
    , ( "responseListField", EventField.encode (E.maybe E.string) data.responseListField )
    , ( "responseItemId", EventField.encode (E.maybe E.string) data.responseItemId )
    , ( "responseItemTemplate", EventField.encode E.string data.responseItemTemplate )
    ]


init : EditIntegrationApiEventData
init =
    { id = EventField.empty
    , name = EventField.empty
    , props = EventField.empty
    , logo = EventField.empty
    , itemUrl = EventField.empty
    , annotations = EventField.empty
    , requestMethod = EventField.empty
    , requestUrl = EventField.empty
    , requestHeaders = EventField.empty
    , requestBody = EventField.empty
    , requestEmptySearch = EventField.empty
    , responseListField = EventField.empty
    , responseItemId = EventField.empty
    , responseItemTemplate = EventField.empty
    }


squash : EditIntegrationApiEventData -> EditIntegrationApiEventData -> EditIntegrationApiEventData
squash oldData newData =
    { id = EventField.squash oldData.id newData.id
    , name = EventField.squash oldData.name newData.name
    , props = EventField.squash oldData.props newData.props
    , logo = EventField.squash oldData.logo newData.logo
    , itemUrl = EventField.squash oldData.itemUrl newData.itemUrl
    , annotations = EventField.squash oldData.annotations newData.annotations
    , requestMethod = EventField.squash oldData.requestMethod newData.requestMethod
    , requestUrl = EventField.squash oldData.requestUrl newData.requestUrl
    , requestHeaders = EventField.squash oldData.requestHeaders newData.requestHeaders
    , requestBody = EventField.squash oldData.requestBody newData.requestBody
    , requestEmptySearch = EventField.squash oldData.requestEmptySearch newData.requestEmptySearch
    , responseListField = EventField.squash oldData.responseListField newData.responseListField
    , responseItemId = EventField.squash oldData.responseItemId newData.responseItemId
    , responseItemTemplate = EventField.squash oldData.responseItemTemplate newData.responseItemTemplate
    }
