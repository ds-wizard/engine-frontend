module Common.Api.Models.OpenIdClient exposing
    ( OpenIdClient
    , decoder
    )

import Common.Api.Models.AuthServiceProviderButtonStyle as AuthServiceProviderButtonStyle exposing (AuthServiceProviderButtonStyle)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias OpenIdClient =
    { uuid : Uuid
    , name : String
    , style : AuthServiceProviderButtonStyle
    }


decoder : Decoder OpenIdClient
decoder =
    D.succeed OpenIdClient
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "style" AuthServiceProviderButtonStyle.decoder
