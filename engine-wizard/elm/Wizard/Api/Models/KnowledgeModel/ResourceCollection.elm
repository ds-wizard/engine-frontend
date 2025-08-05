module Wizard.Api.Models.KnowledgeModel.ResourceCollection exposing
    ( ResourceCollection
    , addResourcePageUuid
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias ResourceCollection =
    { uuid : String
    , title : String
    , resourcePageUuids : List String
    , annotations : List Annotation
    }


decoder : Decoder ResourceCollection
decoder =
    D.succeed ResourceCollection
        |> D.required "uuid" D.string
        |> D.required "title" D.string
        |> D.required "resourcePageUuids" (D.list D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


addResourcePageUuid : String -> ResourceCollection -> ResourceCollection
addResourcePageUuid resourcePageUuid resourceCollection =
    { resourceCollection | resourcePageUuids = resourceCollection.resourcePageUuids ++ [ resourcePageUuid ] }
