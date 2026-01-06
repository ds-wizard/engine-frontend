module Wizard.Pages.Projects.Detail.Files.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectFile exposing (ProjectFile)
import Wizard.Api.ProjectFiles as ProjectFilesApi
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Detail.Files.Models exposing (Model)
import Wizard.Pages.Projects.Detail.Files.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState projectUuid model =
    case msg of
        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg projectUuid model

        DownloadFile file ->
            ( model
            , Cmd.map (wrapMsg << FileDownloaderMsg)
                (FileDownloader.fetchFile (AppState.toServerInfo appState) (ProjectFilesApi.fileUrl file.project.uuid file.uuid))
            )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowHideDeleteFile file ->
            ( { model | projectFileToBeDeleted = file, deletingProjectFile = ActionResult.Unset }, Cmd.none )

        DeleteFileConfirm ->
            case model.projectFileToBeDeleted of
                Just file ->
                    ( { model | deletingProjectFile = ActionResult.Loading }
                    , ProjectFilesApi.delete appState file.project.uuid file.uuid (wrapMsg << DeleteFileCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteFileCompleted result ->
            case result of
                Ok _ ->
                    ( { model | projectFileToBeDeleted = Nothing, deletingProjectFile = ActionResult.Unset }
                    , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | deletingProjectFile = ApiError.toActionResult appState (gettext "File could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg ProjectFile -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg projectUuid model =
    let
        ( questionnaireFiles, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState projectUuid) appState listingMsg model.projectFiles
    in
    ( { model | projectFiles = questionnaireFiles }
    , cmd
    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Listing.UpdateConfig ProjectFile
listingUpdateConfig wrapMsg appState projectUuid =
    { getRequest = ProjectsApi.getFiles appState projectUuid
    , getError = gettext "Unable to get project files." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectsDetailFilesWithFilters projectUuid
    }
