module Wizard.Api.Models.DocumentTemplateLocale exposing
    ( DocumentTemplateLocale
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias DocumentTemplateLocale =
    { uuid : Uuid
    , code : String
    , name : String
    }


decoder : Decoder DocumentTemplateLocale
decoder =
    D.succeed DocumentTemplateLocale
        |> D.required "uuid" Uuid.decoder
        |> D.required "code" D.string
        |> D.required "name" D.string
