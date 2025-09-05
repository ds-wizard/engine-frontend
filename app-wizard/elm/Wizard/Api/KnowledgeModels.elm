module Wizard.Api.KnowledgeModels exposing
    ( fetchAsString
    , fetchPreview
    )

import Common.Api.Request as Request exposing (ToMsg)
import Json.Encode as E
import Json.Encode.Extra as E
import Wizard.Api.Models.Event as Event exposing (Event)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Data.AppState as AppState exposing (AppState)


fetchPreview : AppState -> Maybe String -> List Event -> List String -> ToMsg KnowledgeModel msg -> Cmd msg
fetchPreview appState packageId events tagUuids =
    let
        body =
            E.object
                [ ( "packageId", E.maybe E.string packageId )
                , ( "events", E.list Event.encode events )
                , ( "tagUuids", E.list E.string tagUuids )
                ]
    in
    Request.post (AppState.toServerInfo appState) "/knowledge-models/preview" KnowledgeModel.decoder body


fetchAsString : AppState -> String -> List String -> ToMsg String msg -> Cmd msg
fetchAsString appState packageId tagUuids =
    let
        body =
            E.object
                [ ( "packageId", E.string packageId )
                , ( "events", E.list E.string [] )
                , ( "tagUuids", E.list E.string tagUuids )
                ]
    in
    Request.postAsString (AppState.toServerInfo appState) "/knowledge-models/preview" body
