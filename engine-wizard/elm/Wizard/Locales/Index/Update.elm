module Wizard.Locales.Index.Update exposing (fetchData, update)

import ActionResult
import Gettext exposing (gettext)
import Shared.Api.Locales as LocalesApi
import Shared.Data.Locale exposing (Locale)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (applyResultTransformCmd, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.Locales.Index.Models exposing (Model)
import Wizard.Locales.Index.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeleteLocale locale ->
            ( { model | localeToBeDeleted = locale, deletingLocale = ActionResult.Unset }, Cmd.none )

        DeleteLocale ->
            handleDeleteLocale wrapMsg appState model

        DeleteLocaleCompleted result ->
            deleteLocaleCompleted appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        SetEnabled enabled locale ->
            ( { model | changingLocale = ActionResult.Loading }
            , LocalesApi.setEnabled locale enabled appState (wrapMsg << ChangeLocaleCompleted)
            )

        SetDefault locale ->
            ( { model | changingLocale = ActionResult.Loading }
            , LocalesApi.setDefaultLocale locale appState (wrapMsg << ChangeLocaleCompleted)
            )

        ChangeLocaleCompleted result ->
            applyResultTransformCmd appState
                { setResult = \r m -> { m | changingLocale = r }
                , defaultError = gettext "Unable to change locale." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = always (gettext "Locale was changed successfully." appState.locale)
                , cmd = Ports.refresh ()
                }

        ExportLocale locale ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile appState (LocalesApi.exportLocaleUrl locale.id appState)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeleteLocale : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteLocale wrapMsg appState model =
    case model.localeToBeDeleted of
        Just locale ->
            ( { model | deletingLocale = ActionResult.Loading }
            , Cmd.map wrapMsg <|
                LocalesApi.deleteLocale locale.organizationId locale.localeId appState DeleteLocaleCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deleteLocaleCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteLocaleCompleted appState model result =
    case result of
        Ok _ ->
            ( model
            , Ports.refresh ()
            )

        Err error ->
            ( { model | deletingLocale = ApiError.toActionResult appState (gettext "Locale could not be deleted." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Locale -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( locales, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.locales
    in
    ( { model | locales = locales }
    , cmd
    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Locale
listingUpdateConfig wrapMsg appState =
    { getRequest = LocalesApi.getLocales
    , getError = gettext "Unable to get locales." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.localesIndexWithFilters
    }
