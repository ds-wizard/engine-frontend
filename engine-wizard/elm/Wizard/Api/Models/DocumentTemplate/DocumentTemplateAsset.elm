module Wizard.Api.Models.DocumentTemplate.DocumentTemplateAsset exposing
    ( DocumentTemplateAsset
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Uuid exposing (Uuid)


type alias DocumentTemplateAsset =
    { uuid : Uuid
    , fileName : String
    , contentType : String
    , url : String
    , urlExpiration : Time.Posix
    }


decoder : Decoder DocumentTemplateAsset
decoder =
    D.succeed DocumentTemplateAsset
        |> D.required "uuid" Uuid.decoder
        |> D.required "fileName" D.string
        |> D.required "contentType" D.string
        |> D.required "url" D.string
        |> D.required "urlExpiration" D.datetime


encode : DocumentTemplateAsset -> E.Value
encode asset =
    E.object
        [ ( "fileName", E.string asset.fileName )
        ]
