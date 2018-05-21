module KMEditor.Common.Models.EventsTest exposing (..)

import Expect
import Json.Decode as Decode exposing (..)
import KMEditor.Common.Models.Events exposing (..)
import Test exposing (..)


eventDecoderTests : Test
eventDecoderTests =
    describe "eventDecoderTests"
        [ test "should decode EditKnowledgeModelEvent" <|
            \_ ->
                let
                    rawEvent =
                        """
                        {
                            "eventType": "EditKnowledgeModelEvent",
                            "uuid": "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac",
                            "path": [],
                            "kmUuid": "aad436a7-c8a5-4237-a2bd-34decdf26a1f",
                            "name": {
                                "changed": true,
                                "value": "My Knowledge Model"
                            },
                            "chapterIds": {
                                "changed": false
                            }
                        }
                        """

                    expectedEvent =
                        EditKnowledgeModelEvent
                            { kmUuid = "aad436a7-c8a5-4237-a2bd-34decdf26a1f"
                            , name =
                                { changed = True
                                , value = Just "My Knowledge Model"
                                }
                            , chapterIds =
                                { changed = False
                                , value = Nothing
                                }
                            }
                            { uuid = "79d1e7b4-c2d8-49ff-8293-dfcfdb6da6ac"
                            , path = []
                            }
                in
                case decodeString eventDecoder rawEvent of
                    Ok event ->
                        Expect.equal event expectedEvent

                    Err err ->
                        Expect.fail err
        ]
