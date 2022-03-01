module Shared.Data.Template.TemplateFormat exposing
    ( TemplateFormat
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


type alias TemplateFormat =
    { uuid : Uuid
    , name : String
    , shortName : String
    , color : String
    , icon : String
    , isPdf : Bool
    }


decoder : Decoder TemplateFormat
decoder =
    D.succeed TemplateFormat
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "shortName" D.string
        |> D.required "color" D.string
        |> D.required "icon" D.string
        |> D.optional "isPdf" D.bool False
