module Wizard.Api.Models.TokenResponse exposing
    ( TokenResponse(..)
    , decoder
    )

import Common.Api.Models.UserFromExternal as UserFromExternal exposing (UserFromExternal)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time


type TokenResponse
    = Token String Time.Posix
    | CodeRequired
    | ConsentsRequired String
    | IdentityLinked
    | CompleteRegistrationRequired UserFromExternal
    | EmailVerificationRequired


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

                    "IdentityLinked" ->
                        D.succeed IdentityLinked

                    "CompleteRegistrationRequired" ->
                        D.map CompleteRegistrationRequired UserFromExternal.decoder

                    "EmailVerificationRequired" ->
                        D.succeed EmailVerificationRequired

                    _ ->
                        D.fail <| "Unexpected token response type " ++ type_
            )
