module Shared.Data.DocumentTemplate.DocumentTemplateAllowedPackage exposing
    ( DocumentTemplateAllowedPackage
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias DocumentTemplateAllowedPackage =
    { kmId : Maybe String
    , maxVersion : Maybe String
    , minVersion : Maybe String
    , orgId : Maybe String
    }


decoder : Decoder DocumentTemplateAllowedPackage
decoder =
    D.succeed DocumentTemplateAllowedPackage
        |> D.required "kmId" (D.maybe D.string)
        |> D.required "maxVersion" (D.maybe D.string)
        |> D.required "minVersion" (D.maybe D.string)
        |> D.required "orgId" (D.maybe D.string)
