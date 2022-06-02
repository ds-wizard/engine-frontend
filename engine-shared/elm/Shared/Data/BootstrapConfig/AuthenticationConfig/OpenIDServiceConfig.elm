module Shared.Data.BootstrapConfig.AuthenticationConfig.OpenIDServiceConfig exposing
    ( OpenIDServiceConfig
    , Style
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias OpenIDServiceConfig =
    { id : String
    , name : String
    , style : Style
    }


type alias Style =
    { background : Maybe String
    , color : Maybe String
    , icon : Maybe String
    }


decoder : Decoder OpenIDServiceConfig
decoder =
    D.succeed OpenIDServiceConfig
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "style" styleConfigDecoder


styleConfigDecoder : Decoder Style
styleConfigDecoder =
    D.succeed Style
        |> D.required "background" (D.maybe D.string)
        |> D.required "color" (D.maybe D.string)
        |> D.required "icon" (D.maybe D.string)
