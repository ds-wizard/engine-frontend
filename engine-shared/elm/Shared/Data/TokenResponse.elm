module Shared.Data.TokenResponse exposing (TokenResponse(..), decoder, toToken)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Token exposing (Token)


type TokenResponse
    = Token String
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

                    "CodeRequired" ->
                        D.succeed CodeRequired

                    _ ->
                        D.fail <| "Unexpected token response type " ++ type_
            )


toToken : TokenResponse -> Maybe Token
toToken response =
    case response of
        Token token ->
            Just { token = token }

        CodeRequired ->
            Nothing
