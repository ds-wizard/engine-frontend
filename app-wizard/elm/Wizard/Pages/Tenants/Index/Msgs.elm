module Wizard.Pages.Tenants.Index.Msgs exposing (Msg(..))

import Wizard.Api.Models.Tenant exposing (Tenant)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg Tenant)
