module Shared.Data.KnowledgeModel.TagTest exposing (tagDecoderTest)

import Dict
import Shared.Data.KnowledgeModel.Tag as Tag
import Test exposing (..)
import TestUtils exposing (expectDecoder)


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
                            "annotations": {}
                        }
                        """

                    expected =
                        { uuid = "b5b6ed23-2afa-11e9-b210-d663bd873d93"
                        , name = "Science"
                        , description = Nothing
                        , color = "#F5A623"
                        , annotations = Dict.empty
                        }
                in
                expectDecoder Tag.decoder raw expected
        ]
