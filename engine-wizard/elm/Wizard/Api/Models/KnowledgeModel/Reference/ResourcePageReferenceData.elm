module Wizard.Api.Models.KnowledgeModel.Reference.ResourcePageReferenceData exposing
    ( ResourcePageReferenceData
    , decoder
    , toLabel
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)
import Wizard.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)


type alias ResourcePageReferenceData =
    { uuid : String
    , resourcePageUuid : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder ResourcePageReferenceData
decoder =
    D.succeed ResourcePageReferenceData
        |> D.required "uuid" D.string
        |> D.required "resourcePageUuid" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)


toLabel : List ResourcePage -> ResourcePageReferenceData -> String
toLabel resourcePages data =
    case List.head <| List.filter (\rp -> Just rp.uuid == data.resourcePageUuid) resourcePages of
        Just resourcePage ->
            resourcePage.title

        Nothing ->
            ""
