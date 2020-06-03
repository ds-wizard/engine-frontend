module Shared.Data.KnowledgeModel.KnowledgeModelEntities exposing (KnowledgeModelEntities, decoder)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.Tag as Tag exposing (Tag)


type alias KnowledgeModelEntities =
    { tags : Dict String Tag
    }


decoder : Decoder KnowledgeModelEntities
decoder =
    D.succeed KnowledgeModelEntities
        |> D.required "tags" (D.dict Tag.decoder)
