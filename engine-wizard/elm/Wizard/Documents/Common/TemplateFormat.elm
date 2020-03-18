module Wizard.Documents.Common.TemplateFormat exposing (TemplateFormat, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


type alias TemplateFormat =
    { uuid : String
    , name : String
    , icon : String
    }


decoder : Decoder TemplateFormat
decoder =
    D.succeed TemplateFormat
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "icon" D.string
