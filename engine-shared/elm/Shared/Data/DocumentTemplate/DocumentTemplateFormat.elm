module Shared.Data.DocumentTemplate.DocumentTemplateFormat exposing
    ( DocumentTemplateFormat
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias DocumentTemplateFormat =
    { uuid : Uuid
    , name : String
    , icon : String
    }


decoder : Decoder DocumentTemplateFormat
decoder =
    D.succeed DocumentTemplateFormat
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "icon" D.string
