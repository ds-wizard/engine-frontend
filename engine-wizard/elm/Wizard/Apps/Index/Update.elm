module Wizard.Apps.Index.Update exposing (fetchData, update)

import Dict
import Shared.Api.Apps as AppsApi
import Shared.Data.App exposing (App)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (stringToBool)
import Wizard.Apps.Index.Models exposing (Model)
import Wizard.Apps.Index.Msgs exposing (Msg(..))
import Wizard.Apps.Routes exposing (indexRouteEnabledFilterId)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg App -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( apps, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState model) appState listingMsg model.apps
    in
    ( { model | apps = apps }
    , cmd
    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Listing.UpdateConfig App
listingUpdateConfig wrapMsg appState model =
    let
        enabled =
            Maybe.map stringToBool <|
                Dict.get indexRouteEnabledFilterId model.apps.filters.values
    in
    { getRequest = AppsApi.getApps { enabled = enabled }
    , getError = lg "apiError.apps.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.appsIndexWithFilters model.apps.filters
    }
