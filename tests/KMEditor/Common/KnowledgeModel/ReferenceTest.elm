module KMEditor.Common.KnowledgeModel.ReferenceTest exposing (referenceDecoderTest)

import KMEditor.Common.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Test exposing (..)
import TestUtils exposing (expectDecoder)


referenceDecoderTest : Test
referenceDecoderTest =
    describe "referenceDecoder"
        [ test "should decode ResourcePageReference" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "referenceType": "ResourcePageReference",
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "shortUuid": "atq"
                        }
                        """

                    expected =
                        ResourcePageReference
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , shortUuid = "atq"
                            }
                in
                expectDecoder Reference.decoder raw expected
        , test "should decode URLReference" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "referenceType": "URLReference",
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "url": "http://example.com",
                            "label": "See also"
                        }
                        """

                    expected =
                        URLReference
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , url = "http://example.com"
                            , label = "See also"
                            }
                in
                expectDecoder Reference.decoder raw expected
        , test "should decode CrossReference" <|
            \_ ->
                let
                    raw =
                        """
                        {
                            "referenceType": "CrossReference",
                            "uuid": "8a703cfa-450f-421a-8819-875619ccb54d",
                            "targetUuid": "64217c4e-50b3-4230-9224-bf65c4220ab6",
                            "description": "See also"
                        }
                        """

                    expected =
                        CrossReference
                            { uuid = "8a703cfa-450f-421a-8819-875619ccb54d"
                            , targetUuid = "64217c4e-50b3-4230-9224-bf65c4220ab6"
                            , description = "See also"
                            }
                in
                expectDecoder Reference.decoder raw expected
        ]
