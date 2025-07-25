module Wizard.Api.Models.KnowledgeModel.ChapterTest exposing (chapterDecoderTest)

import Test exposing (Test, describe, test)
import TestUtils exposing (expectDecoder)
import Wizard.Api.Models.KnowledgeModel.Chapter as Chapter


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
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Chapter 1"
                        , text = Just "This chapter is empty"
                        , questionUuids = []
                        , annotations = []
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
                            "annotations": []
                        }
                        """

                    expected =
                        { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                        , title = "Chapter 1"
                        , text = Nothing
                        , questionUuids = [ "2e4307b9-93b8-4617-b8d1-ba0fa9f15e04" ]
                        , annotations = []
                        }
                in
                expectDecoder Chapter.decoder raw expected
        ]
