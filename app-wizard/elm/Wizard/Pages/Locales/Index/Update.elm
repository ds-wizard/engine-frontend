module Wizard.Pages.Locales.Index.Update exposing (fetchData, update)

import ActionResult
import Gettext exposing (gettext)
import Shared.Components.FileDownloader as FileDownloader
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Locales as LocalesApi
import Wizard.Api.Models.Locale exposing (Locale)
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Locales.Index.Models exposing (Model)
import Wizard.Pages.Locales.Index.Msgs exposing (Msg(..))
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
            , LocalesApi.setEnabled appState locale enabled (wrapMsg << ChangeLocaleCompleted)
            )

        SetDefault locale ->
            ( { model | changingLocale = ActionResult.Loading }
            , LocalesApi.setDefaultLocale appState locale (wrapMsg << ChangeLocaleCompleted)
            )

        ChangeLocaleCompleted result ->
            RequestHelpers.applyResultTransformCmd
                { setResult = \r m -> { m | changingLocale = r }
                , defaultError = gettext "Unable to change locale." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                , transform = always (gettext "Locale was changed successfully." appState.locale)
                , cmd = Ports.refresh ()
                , locale = appState.locale
                }

        ExportLocale locale ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (LocalesApi.exportLocaleUrl appState locale.id)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeleteLocale : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteLocale wrapMsg appState model =
    case model.localeToBeDeleted of
        Just locale ->
            ( { model | deletingLocale = ActionResult.Loading }
            , Cmd.map wrapMsg <|
                LocalesApi.deleteLocale appState locale.organizationId locale.localeId DeleteLocaleCompleted
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
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
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
    { getRequest = LocalesApi.getLocales appState
    , getError = gettext "Unable to get locales." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.localesIndexWithFilters
    }
