module Shared.Error.ServerErrorTest exposing (errorDecoderTests)

import Expect
import Json.Decode exposing (..)
import Shared.Error.ServerError as ServerError
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
                        case decodeString ServerError.decoder rawError of
                            Ok error ->
                                error.message

                            Err _ ->
                                ""
                in
                Expect.equal "User could not be created" message
        , test "should decode field errors" <|
            \_ ->
                let
                    rawError =
                        "{\"status\":400,\"error\":\"Bad Request\",\"message\":\"\",\"fieldErrors\":[[\"field1\", \"error1\"], [\"field2\", \"error2\"]]}"

                    fieldErrors =
                        case decodeString ServerError.decoder rawError of
                            Ok error ->
                                error.fieldErrors

                            Err _ ->
                                []

                    expectedFieldErrors =
                        [ ( "field1", "error1" ), ( "field2", "error2" ) ]
                in
                Expect.equal expectedFieldErrors fieldErrors
        ]
