module Wizard.Pages.DocumentTemplateEditors.Index.Subcriptions exposing (subscriptions)

import Wizard.Components.Listing.Subscriptions as Listing
import Wizard.Pages.DocumentTemplateEditors.Index.Models exposing (Model)
import Wizard.Pages.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.documentTemplateDrafts
