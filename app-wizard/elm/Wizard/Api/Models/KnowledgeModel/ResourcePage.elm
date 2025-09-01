module Wizard.Api.Models.KnowledgeModel.ResourcePage exposing
    ( ResourcePage
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias ResourcePage =
    { uuid : String
    , title : String
    , content : String
    , annotations : List Annotation
    }


decoder : Decoder ResourcePage
decoder =
    D.succeed ResourcePage
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "content" D.string
        |> D.required "annotations" (D.list Annotation.decoder)
