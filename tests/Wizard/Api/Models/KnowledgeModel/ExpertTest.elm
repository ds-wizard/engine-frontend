module Wizard.Api.Models.KnowledgeModel.ExpertTest exposing (expertDecoderTest)

import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Expert as Expert


expertDecoderTest : Test
expertDecoderTest =
    describe "expertDecoder"
        [ test "should decode expert" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "name": "John Example",
                            "email": "expert@example.com",
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , name = "John Example"
                        , email = "expert@example.com"
                        , annotations = []
                        }
                in
                expectDecoder Expert.decoder raw expected
        ]
