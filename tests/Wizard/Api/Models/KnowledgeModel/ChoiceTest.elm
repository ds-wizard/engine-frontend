module Wizard.Api.Models.KnowledgeModel.ChoiceTest exposing (choiceDecoderTest)

import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Choice as Choice


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
                            "label": "Choice",
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , label = "Choice"
                        , annotations = []
                        }
                in
                expectDecoder Choice.decoder raw expected
        ]
