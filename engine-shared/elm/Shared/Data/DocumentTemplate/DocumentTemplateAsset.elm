module Shared.Data.DocumentTemplate.DocumentTemplateAsset exposing
    ( DocumentTemplateAsset
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias DocumentTemplateAsset =
    { uuid : Uuid
    , fileName : String
    , contentType : String
    , url : String
    }


decoder : Decoder DocumentTemplateAsset
decoder =
    D.succeed DocumentTemplateAsset
        |> D.required "uuid" Uuid.decoder
        |> D.required "fileName" D.string
        |> D.required "contentType" D.string
        |> D.required "url" D.string
