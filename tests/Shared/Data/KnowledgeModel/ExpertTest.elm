module Shared.Data.KnowledgeModel.ExpertTest exposing (expertDecoderTest)

import Dict
import Shared.Data.KnowledgeModel.Expert as Expert
import Test exposing (..)
import TestUtils exposing (expectDecoder)


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
