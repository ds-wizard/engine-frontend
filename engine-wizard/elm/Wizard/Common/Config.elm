module Wizard.Common.Config exposing
    ( Config
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Common.Config.AffiliationConfig as AffiliationConfig exposing (AffiliationConfig)
import Wizard.Common.Config.ClientConfig as ClientConfig exposing (ClientConfig)
import Wizard.Common.Config.FeaturesConfig as FeaturesConfig exposing (FeaturesConfig)
import Wizard.Common.Config.InfoConfig as InfoConfig exposing (InfoConfig)


type alias Config =
    { affiliation : AffiliationConfig
    , client : ClientConfig
    , features : FeaturesConfig
    , info : InfoConfig
    }


decoder : Decoder Config
decoder =
    D.succeed Config
        |> D.required "affiliation" AffiliationConfig.decoder
        |> D.required "client" ClientConfig.decoder
        |> D.required "features" FeaturesConfig.decoder
        |> D.required "info" InfoConfig.decoder


default : Config
default =
    { affiliation = AffiliationConfig.default
    , client = ClientConfig.default
    , features = FeaturesConfig.default
    , info = InfoConfig.default
    }
