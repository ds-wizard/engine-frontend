module Wizard.Tenants.Index.Msgs exposing (Msg(..))

import Wizard.Api.Models.Tenant exposing (Tenant)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg Tenant)
