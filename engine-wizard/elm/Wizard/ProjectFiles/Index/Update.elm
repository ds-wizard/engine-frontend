module Wizard.ProjectFiles.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult
import Gettext exposing (gettext)
import Shared.Api.QuestionnaireFiles as QuestionnaireFilesApi
import Shared.Data.QuestionnaireFile exposing (QuestionnaireFile)
import Shared.Error.ApiError as ApiError
import Shared.Utils exposing (dispatch)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.Msgs
import Wizard.ProjectFiles.Index.Models exposing (Model)
import Wizard.ProjectFiles.Index.Msgs exposing (Msg(..))
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
                    ( { model | questionnaireFileToBeDeleted = Nothing }
                    , dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | deletingQuestionnaireFile = ApiError.toActionResult appState (gettext "File could not be deleted." appState.locale) error }
                    , getResultCmd Wizard.Msgs.logoutMsg result
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
    { getRequest = QuestionnaireFilesApi.getQuestionnaireFiles
    , getError = gettext "Unable to get project files." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectFilesIndexWithFilters
    }
