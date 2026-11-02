module Common.Api.KnowledgeModels exposing
    ( fetchAsString
    , fetchPreview
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.Event as Event exposing (Event)
import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Params as Params
import Common.Api.Request as Request exposing (ToMsg)
import Json.Encode as E
import Json.Encode.Extra as E
import Uuid exposing (Uuid)


fetchPreview : Context ctx -> Maybe Uuid -> List Event -> List String -> ToMsg KnowledgeModel msg -> Cmd msg
fetchPreview appState kmPackageUuid events tagUuids =
    let
        body =
            E.object
                [ ( "knowledgeModelPackageUuid", E.maybe Uuid.encode kmPackageUuid )
                , ( "events", E.list Event.encode events )
                , ( "tagUuids", E.list E.string tagUuids )
                ]
    in
    Request.post (Context.toServerInfo appState) (Params.apply appState "POST" "/knowledge-models/preview") KnowledgeModel.decoder body


fetchAsString : Context ctx -> Uuid -> List String -> ToMsg String msg -> Cmd msg
fetchAsString appState kmPackageUuid tagUuids =
    let
        body =
            E.object
                [ ( "knowledgeModelPackageUuid", Uuid.encode kmPackageUuid )
                , ( "events", E.list E.string [] )
                , ( "tagUuids", E.list E.string tagUuids )
                ]
    in
    Request.postAsString (Context.toServerInfo appState) (Params.apply appState "POST" "/knowledge-models/preview") body
