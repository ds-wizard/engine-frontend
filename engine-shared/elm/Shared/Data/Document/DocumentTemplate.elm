module Shared.Data.Document.DocumentTemplate exposing
    ( DocumentTemplate
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Uuid exposing (Uuid)


type alias DocumentTemplate =
    { id : String
    , name : String
    , formats : List TemplateFormat
    }


decoder : Decoder DocumentTemplate
decoder =
    D.succeed DocumentTemplate
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "formats" (D.list TemplateFormat.decoder)
