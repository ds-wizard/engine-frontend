module Shared.Data.KnowledgeModel exposing (KnowledgeModel, decoder, getTags)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.KnowledgeModel.KnowledgeModelEntities as KnowledgeModelEntities exposing (KnowledgeModelEntities)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)


type alias KnowledgeModel =
    { uuid : String
    , name : String
    , tagUuids : List String
    , entities : KnowledgeModelEntities
    }


decoder : Decoder KnowledgeModel
decoder =
    D.succeed KnowledgeModel
        |> D.required "uuid" D.string
        |> D.required "name" D.string
        |> D.required "tagUuids" (D.list D.string)
        |> D.required "entities" KnowledgeModelEntities.decoder


getTags : KnowledgeModel -> List Tag
getTags km =
    resolveEntities km.entities.tags km.tagUuids


resolveEntities : Dict String a -> List String -> List a
resolveEntities entities =
    List.filterMap (\uuid -> Dict.get uuid entities)
