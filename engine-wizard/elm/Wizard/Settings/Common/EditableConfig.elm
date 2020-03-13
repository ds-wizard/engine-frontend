module Wizard.Settings.Common.EditableConfig exposing (EditableConfig, decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Wizard.Settings.Common.EditableClientConfig as EditableClientConfig exposing (EditableClientConfig)
import Wizard.Settings.Common.EditableFeaturesConfig as EditableFeaturesConfig exposing (EditableFeaturesConfig)


type alias EditableConfig =
    { features : EditableFeaturesConfig
    , client : EditableClientConfig
    }


decoder : Decoder EditableConfig
decoder =
    D.succeed EditableConfig
        |> D.required "features" EditableFeaturesConfig.decoder
        |> D.required "client" EditableClientConfig.decoder


encode : EditableConfig -> E.Value
encode config =
    E.object
        [ ( "features", EditableFeaturesConfig.encode config.features )
        , ( "client", EditableClientConfig.encode config.client )
        ]
