module KMEditor.Common.KnowledgeModel.IntegrationTest exposing (integrationDecoderTest)

import Dict
import KMEditor.Common.KnowledgeModel.Integration as Integration
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
                            "responseIdField": "id",
                            "responseNameField": "title",
                            "itemUrl": "http://example.com/${id}"
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
                        , responseIdField = "id"
                        , responseNameField = "title"
                        , itemUrl = "http://example.com/${id}"
                        }
                in
                expectDecoder Integration.decoder raw expected
        ]
