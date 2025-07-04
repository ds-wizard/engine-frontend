module Wizard.Api.Models.KnowledgeModel.IntegrationTest exposing (integrationDecoderTest)

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
                            "uuid": "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9",
                            "id": "service",
                            "name": "Service",
                            "props": ["kind", "category"],
                            "logo": "data:image/png;base64,...",
                            "itemUrl": "http://example.com/${id}",
                            "requestMethod": "GET",
                            "requestUrl": "/",
                            "requestHeaders": [{"key": "X_USER", "value": "user"}],
                            "requestBody": "{}",
                            "requestEmptySearch": false,
                            "responseListField": null,
                            "responseItemId": "{{id}}",
                            "responseItemTemplate": "{{title}}",
                            "annotations": []
                        }
                        """

                    expected =
                        ApiIntegration
                            { uuid = "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9"
                            , id = "service"
                            , name = "Service"
                            , props = [ "kind", "category" ]
                            , logo = Just "data:image/png;base64,..."
                            , itemUrl = Just "http://example.com/${id}"
                            , annotations = []
                            }
                            { requestMethod = "GET"
                            , requestUrl = "/"
                            , requestHeaders = [ { key = "X_USER", value = "user" } ]
                            , requestBody = "{}"
                            , requestEmptySearch = False
                            , responseListField = Nothing
                            , responseItemId = Just "{{id}}"
                            , responseItemTemplate = "{{title}}"
                            }
                in
                expectDecoder Integration.decoder raw expected
        , test "should decode widget integration" <|
            \_ ->
                let
                    raw =
                        """
                        {
                          "integrationType": "WidgetIntegration",
                          "uuid": "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9",
                          "id": "service",
                          "name": "Service",
                          "props": ["kind", "category"],
                          "logo": "data:image/png;base64,...",
                          "itemUrl": "http://example.com/${id}",
                          "widgetUrl": "http://example.com",
                          "annotations": []
                        }
                        """

                    expected =
                        WidgetIntegration
                            { uuid = "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9"
                            , id = "service"
                            , name = "Service"
                            , props = [ "kind", "category" ]
                            , logo = Just "data:image/png;base64,..."
                            , itemUrl = Just "http://example.com/${id}"
                            , annotations = []
                            }
                            { widgetUrl = "http://example.com"
                            }
                in
                expectDecoder Integration.decoder raw expected
        ]
