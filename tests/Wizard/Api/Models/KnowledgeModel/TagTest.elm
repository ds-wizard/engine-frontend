module Wizard.Api.Models.KnowledgeModel.TagTest exposing (tagDecoderTest)

import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Tag as Tag


tagDecoderTest : Test
tagDecoderTest =
    describe "tagDecoder"
        [ test "should decode tag" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "b5b6ed23-2afa-11e9-b210-d663bd873d93",
                            "name": "Science",
                            "description": null,
                            "color": "#F5A623",
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "b5b6ed23-2afa-11e9-b210-d663bd873d93"
                        , name = "Science"
                        , description = Nothing
                        , color = "#F5A623"
                        , annotations = []
                        }
                in
                expectDecoder Tag.decoder raw expected
        ]
