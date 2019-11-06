module Wizard.Common.Api.KnowledgeModels exposing (fetchPreview)

import Json.Encode as Encode exposing (Value)
import Wizard.Common.Api exposing (ToMsg, jwtFetch)
import Wizard.Common.AppState exposing (AppState)
import Wizard.KMEditor.Common.Events.Event as Event exposing (Event)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)


fetchPreview : Maybe String -> List Event -> List String -> AppState -> ToMsg KnowledgeModel msg -> Cmd msg
fetchPreview packageId events tagUuids =
    let
        data =
            Encode.object
                [ ( "packageId", packageId |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
                , ( "events", Encode.list Event.encode events )
                , ( "tagUuids", Encode.list Encode.string tagUuids )
                ]
    in
    jwtFetch "/knowledge-models/preview" KnowledgeModel.decoder data
