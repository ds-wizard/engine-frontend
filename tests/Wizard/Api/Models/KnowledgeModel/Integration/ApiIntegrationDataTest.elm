module Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationDataTest exposing (getUnknownConfigReferencesTest)

import Dict
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)
import Wizard.Api.Models.KnowledgeModel.Integration.ApiIntegrationData as ApiIntegrationData exposing (ApiIntegrationData)


{-| An API integration referencing variables and secrets in the request URL,
request body and request header values (with and without spaces inside the
`{{ }}`). Only the `KIND` variable and the `URL_SECRET` secret are declared as
available below.
-}
integrationData : ApiIntegrationData
integrationData =
    let
        raw =
            """
            {
                "allowCustomReply": true,
                "annotations": [],
                "name": "Service",
                "requestAllowEmptySearch": true,
                "requestBody": "token={{ secrets.BODY_SECRET }}&extra={{ variables.EXTRA }}",
                "requestHeaders": [
                    { "key": "Authorization", "value": "Bearer {{secrets.HEADER_SECRET}}" },
                    { "key": "X-Category", "value": "{{ variables.CATEGORY }}" }
                ],
                "requestMethod": "GET",
                "requestUrl": "/search?q={{ q }}&key={{ secrets.URL_SECRET }}&type={{ variables.KIND }}",
                "responseItemTemplate": "{{title}}",
                "responseItemTemplateForSelection": null,
                "responseListField": "items",
                "testQ": "item",
                "testResponse": null,
                "testVariables": {},
                "uuid": "aae37504-aec6-4be8-b703-5bcb3502f3e6",
                "variables": ["KIND"]
            }
            """
    in
    Result.withDefault emptyData (D.decodeString ApiIntegrationData.decoder raw)


{-| Fallback so a decode failure surfaces as a test failure rather than a crash.
-}
emptyData : ApiIntegrationData
emptyData =
    { allowCustomReply = False
    , annotations = []
    , name = ""
    , requestAllowEmptySearch = False
    , requestBody = Nothing
    , requestHeaders = []
    , requestMethod = "GET"
    , requestUrl = ""
    , responseItemTemplate = ""
    , responseItemTemplateForSelection = Nothing
    , responseListField = Nothing
    , testQ = ""
    , testResponse = Nothing
    , testVariables = Dict.empty
    , uuid = ""
    , variables = []
    }


getUnknownConfigReferencesTest : Test
getUnknownConfigReferencesTest =
    describe "ApiIntegrationData.getUnknownConfigReferences"
        [ test "reports undeclared variables referenced anywhere in the request configuration" <|
            \_ ->
                (ApiIntegrationData.getUnknownConfigReferences [ "URL_SECRET" ] integrationData).variables
                    |> Expect.equalLists [ "EXTRA", "CATEGORY" ]
        , test "reports unavailable secrets referenced anywhere in the request configuration" <|
            \_ ->
                (ApiIntegrationData.getUnknownConfigReferences [ "URL_SECRET" ] integrationData).secrets
                    |> Expect.equalLists [ "BODY_SECRET", "HEADER_SECRET" ]
        , test "reports nothing when every referenced variable and secret is available" <|
            \_ ->
                let
                    dataWithAllVariables =
                        { integrationData | variables = [ "KIND", "EXTRA", "CATEGORY" ] }

                    references =
                        ApiIntegrationData.getUnknownConfigReferences [ "URL_SECRET", "BODY_SECRET", "HEADER_SECRET" ] dataWithAllVariables
                in
                Expect.all
                    [ \r -> Expect.equalLists [] r.variables
                    , \r -> Expect.equalLists [] r.secrets
                    ]
                    references
        ]
