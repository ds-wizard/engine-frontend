module Shared.Data.TemplateSimple exposing
    ( TemplateSimple
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Template.TemplateFormat as TemplateFormat exposing (TemplateFormat)
import Uuid exposing (Uuid)


type alias TemplateSimple =
    { id : String
    , name : String
    , formats : List TemplateFormat
    }


decoder : Decoder TemplateSimple
decoder =
    D.succeed TemplateSimple
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "formats" (D.list TemplateFormat.decoder)
