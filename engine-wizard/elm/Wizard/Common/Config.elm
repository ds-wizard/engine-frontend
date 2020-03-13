module Wizard.Common.Config exposing
    ( Config
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.ClientConfig as ClientConfig exposing (ClientConfig)
import Wizard.Common.Config.FeaturesConfig as FeaturesConfig exposing (FeaturesConfig)


type alias Config =
    { client : ClientConfig
    , features : FeaturesConfig
    }


decoder : Decoder Config
decoder =
    D.succeed Config
        |> D.required "client" ClientConfig.decoder
        |> D.required "features" FeaturesConfig.decoder


default : Config
default =
    { client = ClientConfig.default
    , features = FeaturesConfig.default
    }
