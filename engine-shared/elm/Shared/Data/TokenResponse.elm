module Shared.Data.TokenResponse exposing
    ( TokenResponse(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time


type TokenResponse
    = Token String Time.Posix
    | CodeRequired
    | ConsentsRequired String


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

                    "ConsentsRequired" ->
                        D.succeed ConsentsRequired
                            |> D.required "hash" D.string

                    _ ->
                        D.fail <| "Unexpected token response type " ++ type_
            )
