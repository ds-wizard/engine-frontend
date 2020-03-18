module Wizard.Documents.Common.Template exposing
    ( Template
    , decoder
    , listDecoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Documents.Common.TemplateFormat as TemplateFormat exposing (TemplateFormat)


type alias Template =
    { uuid : String
    , name : String
    , formats : List TemplateFormat
    }


decoder : Decoder Template
decoder =
    D.succeed Template
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "formats" (D.list TemplateFormat.decoder)


listDecoder : Decoder (List Template)
listDecoder =
    D.list decoder
