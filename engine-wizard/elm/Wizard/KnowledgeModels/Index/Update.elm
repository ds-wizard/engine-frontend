module Wizard.KnowledgeModels.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Packages as PackagesApi
import Shared.Data.Package exposing (Package)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Utils exposing (dispatch)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.KnowledgeModels.Index.Models exposing (Model)
import Wizard.KnowledgeModels.Index.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeletePackage package ->
            ( { model | packageToBeDeleted = package, deletingPackage = Unset }, Cmd.none )

        DeletePackage ->
            handleDeletePackage wrapMsg appState model

        DeletePackageCompleted result ->
            deletePackageCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        UpdatePhase package phase ->
            ( model, PackagesApi.putPackage { package | phase = phase } appState (wrapMsg << UpdatePhaseCompleted) )

        UpdatePhaseCompleted result ->
            case result of
                Ok _ ->
                    ( model
                    , dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | deletingPackage = ApiError.toActionResult appState (gettext "Knowledge model could not be updated." appState.locale) error }
                    , getResultCmd Wizard.Msgs.logoutMsg result
                    )

        ExportPackage package ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile appState (PackagesApi.exportPackageUrl package.id appState)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeletePackage : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePackage wrapMsg appState model =
    case model.packageToBeDeleted of
        Just package ->
            ( { model | deletingPackage = Loading }
            , Cmd.map wrapMsg <|
                PackagesApi.deletePackage package.organizationId package.kmId appState DeletePackageCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deletePackageCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | packageToBeDeleted = Nothing }
            , dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
            )

        Err error ->
            ( { model | deletingPackage = ApiError.toActionResult appState (gettext "Knowledge model could not be deleted." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Package -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( packages, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.packages
    in
    ( { model | packages = packages }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Package
listingUpdateConfig wrapMsg appState =
    { getRequest = PackagesApi.getPackages
    , getError = gettext "Unable to get Knowledge Models." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.knowledgeModelsIndexWithFilters
    }
