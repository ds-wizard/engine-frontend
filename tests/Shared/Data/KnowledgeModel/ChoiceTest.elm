module Shared.Data.KnowledgeModel.ChoiceTest exposing (..)

import Shared.Data.KnowledgeModel.Choice as Choice
import Test exposing (..)
import TestUtils exposing (expectDecoder)


choiceDecoderTest : Test
choiceDecoderTest =
    describe "choiceDecoder"
        [ test "should decode expert" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "label": "Choice"
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Choice"
                        }
                in
                expectDecoder Choice.decoder raw expected
        ]
