module Wizard.Common.Config.AffiliationConfig exposing
    ( AffiliationConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias AffiliationConfig =
    { affiliations : List String
    }


decoder : Decoder AffiliationConfig
decoder =
    D.succeed AffiliationConfig
        |> D.optional "affiliations" (D.list D.string) []


default : AffiliationConfig
default =
    { affiliations = []
    }
