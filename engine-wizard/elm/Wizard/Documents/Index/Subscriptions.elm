module Wizard.Documents.Index.Subscriptions exposing (..)

import ActionResult exposing (ActionResult(..))
import Time
import Wizard.Common.Components.Listing as Listing
import Wizard.Documents.Index.Models exposing (Model, anyDocumentInProgress)
import Wizard.Documents.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.documents of
        Success documents ->
            let
                listingSub =
                    Sub.map ListingMsg <| Listing.subscriptions documents

                refreshSub =
                    if anyDocumentInProgress model then
                        Time.every 1000 (\_ -> RefreshDocuments)

                    else
                        Sub.none
            in
            Sub.batch
                [ listingSub
                , refreshSub
                ]

        _ ->
            Sub.none
