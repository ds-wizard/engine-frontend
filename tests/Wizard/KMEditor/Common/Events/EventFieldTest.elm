module Wizard.KMEditor.Common.Events.EventFieldTest exposing (eventFieldTest)

import Expect exposing (Expectation)
import Json.Decode as D
import Json.Encode as E
import Test exposing (..)
import TestUtils exposing (expectEncodeDecode)
import Wizard.KMEditor.Common.Events.EventField as EventField


eventFieldTest : Test
eventFieldTest =
    describe "EventField"
        [ test "should encode and decode when not changed" <|
            \_ ->
                let
                    eventField =
                        { changed = False
                        , value = Nothing
                        }
                in
                expectEncodeDecode (EventField.encode E.string) (EventField.decoder D.string) eventField
        , test "should encode and decode when changed" <|
            \_ ->
                let
                    eventField =
                        { changed = True
                        , value = Just "My new value"
                        }
                in
                expectEncodeDecode (EventField.encode E.string) (EventField.decoder D.string) eventField
        , test "should encode and decode when changed to null" <|
            \_ ->
                let
                    eventField =
                        { changed = True
                        , value = Nothing
                        }
                in
                expectEncodeDecode (EventField.encode E.string) (EventField.decoder D.string) eventField
        , test "get value when not changed" <|
            \_ ->
                let
                    eventField =
                        { changed = False
                        , value = Nothing
                        }
                in
                Expect.equal Nothing (EventField.getValue eventField)
        , test "get value when changed" <|
            \_ ->
                let
                    eventField =
                        { changed = True
                        , value = Just 12
                        }
                in
                Expect.equal (Just 12) (EventField.getValue eventField)
        , test "get value with default when not changed" <|
            \_ ->
                let
                    eventField =
                        { changed = False
                        , value = Nothing
                        }
                in
                Expect.equal "Default" (EventField.getValueWithDefault eventField "Default")
        , test "get value with default when changed" <|
            \_ ->
                let
                    eventField =
                        { changed = True
                        , value = Just [ 1, 2, 3 ]
                        }
                in
                Expect.equal [ 1, 2, 3 ] (EventField.getValueWithDefault eventField [])
        ]
