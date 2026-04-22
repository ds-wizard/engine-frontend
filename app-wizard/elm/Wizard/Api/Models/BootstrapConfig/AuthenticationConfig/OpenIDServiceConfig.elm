module Wizard.Api.Models.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing
    ( OpenIDServiceConfig
    , decoder
    )

import Common.Api.Models.AuthServiceProviderButtonStyle as AuthServiceProviderButtonStyle exposing (AuthServiceProviderButtonStyle)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias OpenIDServiceConfig =
    { uuid : Uuid
    , name : String
    , style : AuthServiceProviderButtonStyle
    }


decoder : Decoder OpenIDServiceConfig
decoder =
    D.succeed OpenIDServiceConfig
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "style" AuthServiceProviderButtonStyle.decoder
