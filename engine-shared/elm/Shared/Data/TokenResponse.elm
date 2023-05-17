module Shared.Data.TokenResponse exposing (TokenResponse(..), decoder, toToken)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Shared.Data.Token as Token exposing (Token)
import Time


type TokenResponse
    = Token String Time.Posix
    | CodeRequired


decoder : Decoder TokenResponse
decoder =
    D.field "type" D.string
        |> D.andThen
            (\type_ ->
                case type_ of
                    "UserToken" ->
                        D.succeed Token
                            |> D.required "token" D.string
                            |> D.required "expiresAt" D.datetime

                    "CodeRequired" ->
                        D.succeed CodeRequired

                    _ ->
                        D.fail <| "Unexpected token response type " ++ type_
            )


toToken : TokenResponse -> Maybe Token
toToken response =
    case response of
        Token token expiresAt ->
            Just (Token.create token expiresAt)

        CodeRequired ->
            Nothing
