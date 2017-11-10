module KnowledgeModels.Models exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)


type alias KnowledgeModel =
    { kmContainerUuid : String
    , name : String
    , shortName : String
    , parentPackageName : Maybe String
    , parentPackageVersion : Maybe String
    }


knowledgeModelDecoder : Decoder KnowledgeModel
knowledgeModelDecoder =
    decode KnowledgeModel
        |> required "kmContainerUuid" Decode.string
        |> required "name" Decode.string
        |> required "shortName" Decode.string
        |> required "parentPackageName" (Decode.nullable Decode.string)
        |> required "parentPackageVersion" (Decode.nullable Decode.string)


knowledgeModelListDecoder : Decoder (List KnowledgeModel)
knowledgeModelListDecoder =
    Decode.list knowledgeModelDecoder
