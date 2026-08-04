module Common.Api.Models.OpenIdRequest exposing
    ( OpenIdRequestResponse
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias OpenIdRequestResponse =
    { url : String
    , state : String
    }


decoder : Decoder OpenIdRequestResponse
decoder =
    D.succeed OpenIdRequestResponse
        |> D.required "url" D.string
        |> D.required "state" D.string
