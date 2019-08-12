module KMEditor.Common.KnowledgeModel.ExpertTest exposing (expertDecoderTest)

import KMEditor.Common.KnowledgeModel.Expert as Expert
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
                            "email": "expert@example.com"
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , name = "John Example"
                        , email = "expert@example.com"
                        }
                in
                expectDecoder Expert.decoder raw expected
        ]
