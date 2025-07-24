module Wizard.Api.Models.Event.EditIntegrationApiEventData exposing
    ( EditIntegrationApiEventData
    , decoder
    , encode
    , init
    , squash
    )

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.Event.EventField as EventField exposing (EventField)
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair exposing (KeyValuePair)
import Wizard.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)


type alias EditIntegrationApiEventData =
    { allowCustomReply : EventField Bool
    , annotations : EventField (List Annotation)
    , name : EventField String
    , requestAllowEmptySearch : EventField Bool
    , requestBody : EventField (Maybe String)
    , requestHeaders : EventField (List KeyValuePair)
    , requestMethod : EventField String
    , requestUrl : EventField String
    , responseItemTemplate : EventField String
    , responseItemTemplateForSelection : EventField (Maybe String)
    , responseListField : EventField (Maybe String)
    , testQ : EventField String
    , testResponse : EventField (Maybe TypeHintTestResponse)
    , testVariables : EventField (Dict String String)
    , variables : EventField (List String)
    }


decoder : Decoder EditIntegrationApiEventData
decoder =
    D.succeed EditIntegrationApiEventData
        |> D.required "allowCustomReply" (EventField.decoder D.bool)
        |> D.required "annotations" (EventField.decoder (D.list Annotation.decoder))
        |> D.required "name" (EventField.decoder D.string)
        |> D.required "requestAllowEmptySearch" (EventField.decoder D.bool)
        |> D.required "requestBody" (EventField.decoder (D.maybe D.string))
        |> D.required "requestHeaders" (EventField.decoder (D.list KeyValuePair.decoder))
        |> D.required "requestMethod" (EventField.decoder D.string)
        |> D.required "requestUrl" (EventField.decoder D.string)
        |> D.required "responseItemTemplate" (EventField.decoder D.string)
        |> D.required "responseItemTemplateForSelection" (EventField.decoder (D.maybe D.string))
        |> D.required "responseListField" (EventField.decoder (D.maybe D.string))
        |> D.required "testQ" (EventField.decoder D.string)
        |> D.required "testResponse" (EventField.decoder (D.maybe TypeHintTestResponse.decoder))
        |> D.required "testVariables" (EventField.decoder (D.dict D.string))
        |> D.required "variables" (EventField.decoder (D.list D.string))


encode : EditIntegrationApiEventData -> List ( String, D.Value )
encode data =
    [ ( "integrationType", E.string "ApiIntegration" )
    , ( "allowCustomReply", EventField.encode E.bool data.allowCustomReply )
    , ( "annotations", EventField.encode (E.list Annotation.encode) data.annotations )
    , ( "name", EventField.encode E.string data.name )
    , ( "requestAllowEmptySearch", EventField.encode E.bool data.requestAllowEmptySearch )
    , ( "requestBody", EventField.encode (E.maybe E.string) data.requestBody )
    , ( "requestHeaders", EventField.encode (E.list KeyValuePair.encode) data.requestHeaders )
    , ( "requestMethod", EventField.encode E.string data.requestMethod )
    , ( "requestUrl", EventField.encode E.string data.requestUrl )
    , ( "responseItemTemplate", EventField.encode E.string data.responseItemTemplate )
    , ( "responseItemTemplateForSelection", EventField.encode (E.maybe E.string) data.responseItemTemplateForSelection )
    , ( "responseListField", EventField.encode (E.maybe E.string) data.responseListField )
    , ( "testQ", EventField.encode E.string data.testQ )
    , ( "testResponse", EventField.encode (E.maybe TypeHintTestResponse.encode) data.testResponse )
    , ( "testVariables", EventField.encode (E.dict identity E.string) data.testVariables )
    , ( "variables", EventField.encode (E.list E.string) data.variables )
    ]


init : EditIntegrationApiEventData
init =
    { allowCustomReply = EventField.empty
    , annotations = EventField.empty
    , name = EventField.empty
    , requestAllowEmptySearch = EventField.empty
    , requestBody = EventField.empty
    , requestHeaders = EventField.empty
    , requestMethod = EventField.empty
    , requestUrl = EventField.empty
    , responseItemTemplate = EventField.empty
    , responseItemTemplateForSelection = EventField.empty
    , responseListField = EventField.empty
    , testQ = EventField.empty
    , testResponse = EventField.empty
    , testVariables = EventField.empty
    , variables = EventField.empty
    }


squash : EditIntegrationApiEventData -> EditIntegrationApiEventData -> EditIntegrationApiEventData
squash oldData newData =
    { allowCustomReply = EventField.squash oldData.allowCustomReply newData.allowCustomReply
    , annotations = EventField.squash oldData.annotations newData.annotations
    , name = EventField.squash oldData.name newData.name
    , requestAllowEmptySearch = EventField.squash oldData.requestAllowEmptySearch newData.requestAllowEmptySearch
    , requestBody = EventField.squash oldData.requestBody newData.requestBody
    , requestHeaders = EventField.squash oldData.requestHeaders newData.requestHeaders
    , requestMethod = EventField.squash oldData.requestMethod newData.requestMethod
    , requestUrl = EventField.squash oldData.requestUrl newData.requestUrl
    , responseItemTemplate = EventField.squash oldData.responseItemTemplate newData.responseItemTemplate
    , responseItemTemplateForSelection = EventField.squash oldData.responseItemTemplateForSelection newData.responseItemTemplateForSelection
    , responseListField = EventField.squash oldData.responseListField newData.responseListField
    , testQ = EventField.squash oldData.testQ newData.testQ
    , testResponse = EventField.squash oldData.testResponse newData.testResponse
    , testVariables = EventField.squash oldData.testVariables newData.testVariables
    , variables = EventField.squash oldData.variables newData.variables
    }
