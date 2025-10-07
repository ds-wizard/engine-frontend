module Wizard.Pages.Documents.Index.Subscriptions exposing (subscriptions)

import Time
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Subscriptions as ListingSubscriptions
import Wizard.Pages.Documents.Index.Models exposing (Model, anyDocumentInProgress)
import Wizard.Pages.Documents.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        refreshSub =
            if anyDocumentInProgress model then
                Time.every 1000 (\_ -> ListingMsg ListingMsgs.ReloadBackground)

            else
                Sub.none

        listingSub =
            Sub.map ListingMsg <|
                ListingSubscriptions.subscriptions model.documents
    in
    Sub.batch
        [ refreshSub, listingSub ]
