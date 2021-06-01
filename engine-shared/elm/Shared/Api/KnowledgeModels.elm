module Shared.Api.KnowledgeModels exposing (fetchPreview)

import Json.Encode as E
import Json.Encode.Extra as E
import Shared.AbstractAppState exposing (AbstractAppState)
import Shared.Api exposing (ToMsg, jwtOrHttpFetch)
import Shared.Data.Event as Event exposing (Event)
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)


fetchPreview : Maybe String -> List Event -> List String -> AbstractAppState a -> ToMsg KnowledgeModel msg -> Cmd msg
fetchPreview packageId events tagUuids =
    let
        data =
            E.object
                [ ( "packageId", E.maybe E.string packageId )
                , ( "events", E.list Event.encode events )
                , ( "tagUuids", E.list E.string tagUuids )
                ]
    in
    jwtOrHttpFetch "/knowledge-models/preview" KnowledgeModel.decoder data
