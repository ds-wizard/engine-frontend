module Wizard.Pages.ProjectFiles.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Common.Components.FileDownloader as FileDownloader
import Common.Data.ApiError as ApiError
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Task.Extra as Task
import Wizard.Api.Models.QuestionnaireFile exposing (QuestionnaireFile)
import Wizard.Api.QuestionnaireFiles as QuestionnaireFilesApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.ProjectFiles.Index.Models exposing (Model)
import Wizard.Pages.ProjectFiles.Index.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        DownloadFile file ->
            ( model
            , Cmd.map (wrapMsg << FileDownloaderMsg)
                (FileDownloader.fetchFile (AppState.toServerInfo appState) (QuestionnaireFilesApi.fileUrl appState file.questionnaire.uuid file.uuid))
            )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowHideDeleteFile file ->
            ( { model | questionnaireFileToBeDeleted = file, deletingQuestionnaireFile = ActionResult.Unset }, Cmd.none )

        DeleteFileConfirm ->
            case model.questionnaireFileToBeDeleted of
                Just file ->
                    ( { model | deletingQuestionnaireFile = ActionResult.Loading }
                    , QuestionnaireFilesApi.deleteFile appState file.questionnaire.uuid file.uuid (wrapMsg << DeleteFileCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteFileCompleted result ->
            case result of
                Ok _ ->
                    ( { model | questionnaireFileToBeDeleted = Nothing }
                    , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | deletingQuestionnaireFile = ApiError.toActionResult appState (gettext "File could not be deleted." appState.locale) error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg QuestionnaireFile -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( questionnaireFiles, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.questionnaireFiles
    in
    ( { model | questionnaireFiles = questionnaireFiles }
    , cmd
    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig QuestionnaireFile
listingUpdateConfig wrapMsg appState =
    { getRequest = QuestionnaireFilesApi.getQuestionnaireFiles appState
    , getError = gettext "Unable to get project files." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectFilesIndexWithFilters
    }
