module Wizard.Projects.Detail.Files.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Gettext exposing (gettext)
import Shared.Api.QuestionnaireFiles as QuestionnaireFilesApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Data.QuestionnaireFile exposing (QuestionnaireFile)
import Shared.Error.ApiError as ApiError
import Shared.Utils exposing (dispatch)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.Msgs
import Wizard.Projects.Detail.Files.Models exposing (Model)
import Wizard.Projects.Detail.Files.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState questionnaireUuid model =
    case msg of
        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg questionnaireUuid model

        DownloadFile file ->
            ( model
            , Cmd.map (wrapMsg << FileDownloaderMsg)
                (FileDownloader.fetchFile appState (QuestionnaireFilesApi.fileUrl file.questionnaire.uuid file.uuid appState))
            )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )

        ShowHideDeleteFile file ->
            ( { model | questionnaireFileToBeDeleted = file, deletingQuestionnaireFile = ActionResult.Unset }, Cmd.none )

        DeleteFileConfirm ->
            case model.questionnaireFileToBeDeleted of
                Just file ->
                    ( { model | deletingQuestionnaireFile = ActionResult.Loading }
                    , QuestionnaireFilesApi.deleteFile file.questionnaire.uuid file.uuid appState (wrapMsg << DeleteFileCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteFileCompleted result ->
            case result of
                Ok _ ->
                    ( { model | questionnaireFileToBeDeleted = Nothing, deletingQuestionnaireFile = ActionResult.Unset }
                    , dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | deletingQuestionnaireFile = ApiError.toActionResult appState (gettext "File could not be deleted." appState.locale) error }
                    , getResultCmd Wizard.Msgs.logoutMsg result
                    )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg QuestionnaireFile -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg questionnaireUuid model =
    let
        ( questionnaireFiles, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState questionnaireUuid) appState listingMsg model.questionnaireFiles
    in
    ( { model | questionnaireFiles = questionnaireFiles }
    , cmd
    )


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Listing.UpdateConfig QuestionnaireFile
listingUpdateConfig wrapMsg appState questionnaireUuid =
    { getRequest = QuestionnairesApi.getFiles questionnaireUuid
    , getError = gettext "Unable to get project files." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectsDetailFilesWithFilters questionnaireUuid
    }
