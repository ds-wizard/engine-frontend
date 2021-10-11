module Shared.Data.KnowledgeModel.ChapterTest exposing (chapterDecoderTest)

import Dict
import Shared.Data.KnowledgeModel.Chapter as Chapter
import Test exposing (..)
import TestUtils exposing (expectDecoder)


chapterDecoderTest : Test
chapterDecoderTest =
    describe "chapterDecoder"
        [ test "should decode simple chapter" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "title": "Chapter 1",
                            "text": "This chapter is empty",
                            "questionUuids": [],
                            "annotations": {}
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Chapter 1"
                        , text = Just "This chapter is empty"
                        , questionUuids = []
                        , annotations = Dict.empty
                        }
                in
                expectDecoder Chapter.decoder raw expected
        , test "should decode chapter with questions" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "title": "Chapter 1",
                            "text": null,
                            "questionUuids": ["2e4307b9-93b8-4617-b8d1-ba0fa9f15e04"],
                            "annotations": {}
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Chapter 1"
                        , text = Nothing
                        , questionUuids = [ "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04" ]
                        , annotations = Dict.empty
                        }
                in
                expectDecoder Chapter.decoder raw expected
        ]
