module Shared.Data.DocumentTemplate.DocumentTemplateFormatSimple exposing
    ( DocumentTemplateFormatSimple
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias DocumentTemplateFormatSimple =
    { uuid : Uuid
    , name : String
    , icon : String
    }


decoder : Decoder DocumentTemplateFormatSimple
decoder =
    D.succeed DocumentTemplateFormatSimple
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "icon" D.string
