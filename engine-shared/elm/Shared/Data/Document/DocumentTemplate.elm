module Shared.Data.Document.DocumentTemplate exposing
    ( DocumentTemplate
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.DocumentTemplate.DocumentTemplateFormat as DocumentTemplateFormat exposing (DocumentTemplateFormat)


type alias DocumentTemplate =
    { id : String
    , name : String
    , formats : List DocumentTemplateFormat
    }


decoder : Decoder DocumentTemplate
decoder =
    D.succeed DocumentTemplate
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "formats" (D.list DocumentTemplateFormat.decoder)
