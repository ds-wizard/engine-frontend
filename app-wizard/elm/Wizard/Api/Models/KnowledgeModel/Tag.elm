module Wizard.Api.Models.KnowledgeModel.Tag exposing (Tag, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias Tag =
    { uuid : String
    , name : String
    , description : Maybe String
    , color : String
    , annotations : List Annotation
    }


decoder : Decoder Tag
decoder =
    D.succeed Tag
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "description" (D.nullable D.string)
        |> D.required "color" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
