module Shared.Data.KnowledgeModel.PhaseTest exposing (phaseDecoderTest)

import Dict
import Shared.Data.KnowledgeModel.Phase as Phase
import Test exposing (..)
import TestUtils exposing (expectDecoder)


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
                            "annotations": {}
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Phase"
                        , description = Just "This is a phase"
                        , annotations = Dict.empty
                        }
                in
                expectDecoder Phase.decoder raw expected
        ]
