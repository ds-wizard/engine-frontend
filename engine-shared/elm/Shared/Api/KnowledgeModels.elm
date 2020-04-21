module Shared.Api.KnowledgeModels exposing (fetchPreview)

import Json.Encode as E
import Shared.Api exposing (AppStateLike, ToMsg, jwtFetch)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)


fetchPreview : String -> AppStateLike a -> ToMsg KnowledgeModel msg -> Cmd msg
fetchPreview packageId =
    let
        data =
            E.object
                [ ( "packageId", E.string packageId )
                , ( "events", E.list E.string [] )
                , ( "tagUuids", E.list E.string [] )
                ]
    in
    jwtFetch "/knowledge-models/preview" KnowledgeModel.decoder data
