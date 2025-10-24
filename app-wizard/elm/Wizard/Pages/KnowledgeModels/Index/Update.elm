module Wizard.Pages.KnowledgeModels.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Task.Extra as Task
import Wizard.Api.KnowledgeModelPackages as KnowledgeModelPackagesApi
import Wizard.Api.Models.KnowledgeModelPackage exposing (KnowledgeModelPackage)
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.KnowledgeModels.Index.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeletePackage kmPackage ->
            ( { model | kmPackageToBeDeleted = kmPackage, deletingKmPackage = Unset }, Cmd.none )

        DeleteKnowledgeModelPackage ->
            handleDeletePackage wrapMsg appState model

        DeleteKnowledgeModelPackageCompleted result ->
            deletePackageCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        UpdatePhase kmPackage phase ->
            ( model, KnowledgeModelPackagesApi.putKnowledgeModelPackage appState { kmPackage | phase = phase } (wrapMsg << UpdatePhaseCompleted) )

        UpdatePhaseCompleted result ->
            case result of
                Ok _ ->
                    ( model
                    , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | deletingKmPackage = ApiError.toActionResult appState (gettext "Knowledge model could not be updated." appState.locale) error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )

        ExportKnowledgeModelPackage kmPackage ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (KnowledgeModelPackagesApi.exportKnowledgeModelPackageUrl kmPackage.id)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeletePackage : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePackage wrapMsg appState model =
    case model.kmPackageToBeDeleted of
        Just kmPackage ->
            ( { model | deletingKmPackage = Loading }
            , Cmd.map wrapMsg <|
                KnowledgeModelPackagesApi.deleteKnowledgeModelPackage appState kmPackage.organizationId kmPackage.kmId DeleteKnowledgeModelPackageCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deletePackageCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | kmPackageToBeDeleted = Nothing }
            , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
            )

        Err error ->
            ( { model | deletingKmPackage = ApiError.toActionResult appState (gettext "Knowledge model could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg KnowledgeModelPackage -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( packages, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.packages
    in
    ( { model | packages = packages }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig KnowledgeModelPackage
listingUpdateConfig wrapMsg appState =
    { getRequest = KnowledgeModelPackagesApi.getKnowledgeModelPackages appState
    , getError = gettext "Unable to get Knowledge Models." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.knowledgeModelsIndexWithFilters
    }
