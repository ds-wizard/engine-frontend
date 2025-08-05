module Wizard.Api.Models.KnowledgeModel.Integration.CommonIntegrationData exposing (CommonIntegrationData, decoder)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.KnowledgeModel.Annotation as Annotation exposing (Annotation)


type alias CommonIntegrationData =
    { uuid : String
    , id : String
    , name : String
    , props : List String
    , logo : Maybe String
    , itemUrl : Maybe String
    , annotations : List Annotation
    }


decoder : Decoder CommonIntegrationData
decoder =
    D.succeed CommonIntegrationData
        |> D.required "uuid" D.string
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "props" (D.list D.string)
        |> D.required "logo" (D.maybe D.string)
        |> D.required "itemUrl" (D.maybe D.string)
        |> D.required "annotations" (D.list Annotation.decoder)
