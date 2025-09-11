module Common.Data.ServerErrorTest exposing (errorDecoderTests)

import Common.Api.ServerError as ServerError
import Dict
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (Test, describe, test)


errorDecoderTests : Test
errorDecoderTests =
    describe "errorDecoderTests"
        [ test "UserSimpleError" <|
            \_ ->
                let
                    rawError =
                        "{\"type\":\"UserSimpleError\",\"error\":{\"code\":\"error-code\",\"params\":[\"foo\"]}}"

                    result =
                        case decodeString ServerError.decoder rawError of
                            Ok (ServerError.UserSimpleError message) ->
                                Ok message

                            Err _ ->
                                Err ()

                            _ ->
                                Err ()
                in
                Expect.equal result (Ok { code = "error-code", params = [ "foo" ] })
        , test "UserFormError" <|
            \_ ->
                let
                    rawError =
                        "{\"type\":\"UserFormError\",\"formErrors\":[{\"code\":\"form-error-code\",\"params\":[\"foo\"]}],\"fieldErrors\":{\"field\":[{\"code\":\"field-error-code\",\"params\":[\"bar\"]}]}}"

                    result =
                        case decodeString ServerError.decoder rawError of
                            Ok (ServerError.UserFormError error) ->
                                Ok error

                            _ ->
                                Err ()

                    expected =
                        { formErrors = [ { code = "form-error-code", params = [ "foo" ] } ]
                        , fieldErrors =
                            Dict.fromList
                                [ ( "field"
                                  , [ { code = "field-error-code", params = [ "bar" ] } ]
                                  )
                                ]
                        }
                in
                Expect.equal result (Ok expected)
        ]
