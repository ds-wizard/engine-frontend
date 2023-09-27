module Wizard.Tenants.Index.Msgs exposing (Msg(..))

import Shared.Data.Tenant exposing (Tenant)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg Tenant)
