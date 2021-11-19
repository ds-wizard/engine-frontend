module Shared.Data.KnowledgeModel.IntegrationTest exposing (integrationDecoderTest)

import Dict
import Shared.Data.KnowledgeModel.Integration as Integration
import Test exposing (..)
import TestUtils exposing (expectDecoder)


integrationDecoderTest : Test
integrationDecoderTest =
    describe "integrationDecoder"
        [ test "should decode integration" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9",
                            "id": "service",
                            "name": "Service",
                            "props": ["kind", "category"],
                            "logo": "data:image/png;base64,...",
                            "requestMethod": "GET",
                            "requestUrl": "/",
                            "requestHeaders": {"X_USER": "user"},
                            "requestBody": "{}",
                            "responseListField": "",
                            "responseItemId": "{{id}}",
                            "responseItemTemplate": "{{title}}",
                            "responseItemUrl": "http://example.com/${id}",
                            "annotations": {}
                        }
                        """

                    expected =
                        { uuid = "8f831db8-6f7a-42bd-bcd6-7b5174fd1ec9"
                        , id = "service"
                        , name = "Service"
                        , props = [ "kind", "category" ]
                        , logo = "data:image/png;base64,..."
                        , requestMethod = "GET"
                        , requestUrl = "/"
                        , requestHeaders = Dict.fromList [ ( "X_USER", "user" ) ]
                        , requestBody = "{}"
                        , responseListField = ""
                        , responseItemId = "{{id}}"
                        , responseItemTemplate = "{{title}}"
                        , responseItemUrl = "http://example.com/${id}"
                        , annotations = Dict.empty
                        }
                in
                expectDecoder Integration.decoder raw expected
        ]
