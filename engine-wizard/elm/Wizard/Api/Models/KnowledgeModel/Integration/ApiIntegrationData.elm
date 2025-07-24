module Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData exposing
    ( ApiIntegrationData
    , decoder
    , getTestVariableValue
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import List.Extra as List
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair exposing (KeyValuePair)
import Wizard.Api.Models.TypeHintTestResponse as TypeHintTestResponse exposing (TypeHintTestResponse)


type alias ApiIntegrationData =
    { allowCustomReply : Bool
    , annotations : List Annotation
    , name : String
    , requestAllowEmptySearch : Bool
    , requestBody : Maybe String
    , requestHeaders : List KeyValuePair
    , requestMethod : String
    , requestUrl : String
    , responseItemTemplate : String
    , responseItemTemplateForSelection : Maybe String
    , responseListField : Maybe String
    , testQ : String
    , testResponse : Maybe TypeHintTestResponse
    , testVariables : List KeyValuePair
    , uuid : String
    , variables : List String
    }


decoder : Decoder ApiIntegrationData
decoder =
    D.succeed ApiIntegrationData
        |> D.required "allowCustomReply" D.bool
        |> D.required "annotations" (D.list Annotation.decoder)
        |> D.required "name" D.string
        |> D.required "requestAllowEmptySearch" D.bool
        |> D.required "requestBody" (D.maybe D.string)
        |> D.required "requestHeaders" (D.list KeyValuePair.decoder)
        |> D.required "requestMethod" D.string
        |> D.required "requestUrl" D.string
        |> D.required "responseItemTemplate" D.string
        |> D.required "responseItemTemplateForSelection" (D.maybe D.string)
        |> D.required "responseListField" (D.maybe D.string)
        |> D.required "testQ" D.string
        |> D.required "testResponse" (D.maybe TypeHintTestResponse.decoder)
        |> D.required "testVariables" (D.list KeyValuePair.decoder)
        |> D.required "uuid" D.string
        |> D.required "variables" (D.list D.string)


getTestVariableValue : String -> ApiIntegrationData -> Maybe String
getTestVariableValue variableName data =
    data.testVariables
        |> List.find (\kvp -> kvp.key == variableName)
        |> Maybe.map .value
