module Wizard.DocumentTemplateEditors.Index.Subcriptions exposing (subscriptions)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.DocumentTemplateEditors.Index.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.documentTemplateDrafts
