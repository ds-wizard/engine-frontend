module Wizard.Tenants.Index.Update exposing (fetchData, update)

import Wizard.Api.Models.Tenant exposing (Tenant)
import Wizard.Api.Tenants as TenantsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Tenants.Index.Models exposing (Model)
import Wizard.Tenants.Index.Msgs exposing (Msg(..))


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Tenant -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( tenants, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.tenants
    in
    ( { model | tenants = tenants }
    , cmd
    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Tenant
listingUpdateConfig wrapMsg appState =
    { getRequest = TenantsApi.getTenants appState
    , getError = "Unable to get tenants."
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.tenantsIndexWithFilters
    }
