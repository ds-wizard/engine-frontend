module Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData exposing
    ( ApiIntegrationData
    , decoder
    , equalContent
    , getTestVariableValue
    , getUnknownVariables
    )

import Common.Utils.JinjaUtils exposing (JinjaParseResult)
import Dict exposing (Dict)
import Flip exposing (flip)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
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
    , testVariables : Dict String String
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
        |> D.required "testVariables" (D.dict D.string)
        |> D.required "uuid" D.string
        |> D.required "variables" (D.list D.string)


equalContent : ApiIntegrationData -> ApiIntegrationData -> Bool
equalContent data1 data2 =
    (data1.allowCustomReply == data2.allowCustomReply)
        && (data1.name == data2.name)
        && (data1.requestAllowEmptySearch == data2.requestAllowEmptySearch)
        && (data1.requestBody == data2.requestBody)
        && (data1.requestHeaders == data2.requestHeaders)
        && (data1.requestMethod == data2.requestMethod)
        && (data1.requestUrl == data2.requestUrl)
        && (data1.responseItemTemplate == data2.responseItemTemplate)
        && (data1.responseItemTemplateForSelection == data2.responseItemTemplateForSelection)
        && (data1.responseListField == data2.responseListField)
        && (data1.testQ == data2.testQ)
        && (data1.testResponse == data2.testResponse)
        && (data1.testVariables == data2.testVariables)
        && (data1.variables == data2.variables)


getTestVariableValue : String -> ApiIntegrationData -> Maybe String
getTestVariableValue variableName data =
    Dict.get variableName data.testVariables


getUnknownVariables :
    JinjaParseResult
    -> List String
    -> ApiIntegrationData
    ->
        { properties : List String
        , variables : List String
        , secrets : List String
        }
getUnknownVariables result secrets data =
    let
        filterUnknown var =
            not
                ((var == "q")
                    || String.startsWith "variables." var
                    || String.startsWith "secrets." var
                )

        unknownProperties =
            List.filter filterUnknown result.properties

        unknownVariables =
            List.filter (not << flip List.member data.variables) result.variablesNested

        unknownSecrets =
            List.filter (not << flip List.member secrets) result.secretsNested
    in
    { properties = unknownProperties
    , variables = unknownVariables
    , secrets = unknownSecrets
    }
