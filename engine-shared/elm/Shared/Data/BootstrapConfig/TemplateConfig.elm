module Shared.Data.BootstrapConfig.TemplateConfig exposing
    ( TemplateConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Utils exposing (nilUuid)


type alias TemplateConfig =
    { recommendedTemplateUuid : Maybe String
    }


default : TemplateConfig
default =
    { recommendedTemplateUuid = Nothing }


decoder : Decoder TemplateConfig
decoder =
    D.succeed TemplateConfig
        |> D.required "recommendedTemplateUuid" (D.maybe D.string)
