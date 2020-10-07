module Wizard.Templates.Index.Subscriptions exposing (..)

import Wizard.Common.Components.Listing.Subscriptions as Listing
import Wizard.Templates.Index.Models exposing (Model)
import Wizard.Templates.Index.Msgs exposing (Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ListingMsg <|
        Listing.subscriptions model.templates
