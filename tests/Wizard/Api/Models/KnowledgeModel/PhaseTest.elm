module Wizard.Api.Models.KnowledgeModel.PhaseTest exposing (phaseDecoderTest)

import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Phase as Phase


phaseDecoderTest : Test
phaseDecoderTest =
    describe "phaseDecoder"
        [ test "should decode phase" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "title": "Phase",
                            "description": "This is a phase",
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Phase"
                        , description = Just "This is a phase"
                        , annotations = []
                        }
                in
                expectDecoder Phase.decoder raw expected
        ]
