module Common.ApiErrorTest exposing (errorDecoderTests)

import Common.ApiError exposing (errorDecoder)
import Expect
import Json.Decode as Decode exposing (..)
import Test exposing (..)


errorDecoderTests : Test
errorDecoderTests =
    describe "errorDecoderTests"
        [ test "should decode message" <|
            \_ ->
                let
                    rawError =
                        "{\"status\":400,\"error\":\"Bad Request\",\"message\":\"User could not be created\",\"fieldErrors\":[]}"

                    message =
                        case decodeString errorDecoder rawError of
                            Ok error ->
                                error.message

                            Err err ->
                                ""
                in
                Expect.equal "User could not be created" message
        , test "should decode field errors" <|
            \_ ->
                let
                    rawError =
                        "{\"status\":400,\"error\":\"Bad Request\",\"message\":\"\",\"fieldErrors\":[[\"field1\", \"error1\"], [\"field2\", \"error2\"]]}"

                    fieldErrors =
                        case decodeString errorDecoder rawError of
                            Ok error ->
                                error.fieldErrors

                            Err err ->
                                []

                    expectedFieldErrors =
                        [ ( "field1", "error1" ), ( "field2", "error2" ) ]
                in
                Expect.equal expectedFieldErrors fieldErrors
        ]
