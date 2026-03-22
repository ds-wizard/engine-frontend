module Wizard.Api.Models.KnowledgeModel.IntegrationTest exposing (integrationDecoderTest)

import Dict
import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration(..))


integrationDecoderTest : Test
integrationDecoderTest =
    describe "integrationDecoder"
        [ test "should decode API integration" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "integrationType": "ApiIntegration",
                            "allowCustomReply": true,
                            "annotations": [],
                            "name": "Service",
                            "requestAllowEmptySearch": true,
                            "requestBody": "{}",
                            "requestHeaders": [{ "key": "X_USER", "value": "user" }],
                            "requestMethod": "GET",
                            "requestUrl": "/",
                            "responseItemTemplate": "{{title}}",
                            "responseItemTemplateForSelection": null,
                            "responseListField": "items",
                            "testQ": "item",
                            "testResponse": null,
                            "testVariables": {},
                            "uuid": "aae37504-aec6-4be8-b703-5bcb3502f3e6",
                            "variables": ["kind", "category"]
                        }
                        """

                    expected =
                        ApiIntegration
                            { allowCustomReply = True
                            , annotations = []
                            , name = "Service"
                            , requestAllowEmptySearch = True
                            , requestBody = Just "{}"
                            , requestHeaders = [ { key = "X_USER", value = "user" } ]
                            , requestMethod = "GET"
                            , requestUrl = "/"
                            , responseItemTemplate = "{{title}}"
                            , responseItemTemplateForSelection = Nothing
                            , responseListField = Just "items"
                            , testQ = "item"
                            , testResponse = Nothing
                            , testVariables = Dict.empty
                            , uuid = "aae37504-aec6-4be8-b703-5bcb3502f3e6"
                            , variables = [ "kind", "category" ]
                            }
                in
                expectDecoder Integration.decoder raw expected
        ]
