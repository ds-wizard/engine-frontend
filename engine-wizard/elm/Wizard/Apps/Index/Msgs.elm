module Wizard.Apps.Index.Msgs exposing (Msg(..))

import Shared.Data.App exposing (App)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg App)
