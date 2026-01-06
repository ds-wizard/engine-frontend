module Wizard.Pages.Projects.Detail.Documents.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.FileDownloader as FileDownloader
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Uuid exposing (Uuid)
import Wizard.Api.Documents as DocumentsApi
import Wizard.Api.Models.Document exposing (Document)
import Wizard.Api.Models.Submission exposing (Submission)
import Wizard.Api.Models.SubmissionService exposing (SubmissionService)
import Wizard.Api.Projects as ProjectsApi
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Projects.Detail.Documents.Models exposing (Model, addDocumentSubmission)
import Wizard.Pages.Projects.Detail.Documents.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState projectUuid model =
    case msg of
        ShowHideDeleteDocument mbDocument ->
            handleShowHideDeleteDocument model mbDocument

        DeleteDocument ->
            handleDeleteDocument wrapMsg appState model

        DeleteDocumentCompleted result ->
            handleDeleteDocumentCompleted wrapMsg appState projectUuid model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg projectUuid model

        ShowHideSubmitDocument mbDocument ->
            handleShowHideSubmitDocument wrapMsg appState model mbDocument

        GetSubmissionServicesCompleted result ->
            handleGetSubmissionServicesCompleted appState model result

        SelectSubmissionService id ->
            handleSelectSubmissionService model id

        SubmitDocument ->
            handleSubmitDocument wrapMsg appState model

        SubmitDocumentCompleted result ->
            handleSubmitDocumentCompleted appState model result

        SetDocumentErrorModal mbError ->
            ( { model | documentErrorModal = mbError }, Cmd.none )

        SetSubmissionErrorModal mbError ->
            ( { model | submissionErrorModal = mbError }, Cmd.none )

        DownloadDocument document ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (DocumentsApi.downloadDocumentUrl document.uuid)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleShowHideDeleteDocument : Model -> Maybe Document -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteDocument model mbDocument =
    ( { model | documentToBeDeleted = mbDocument, deletingDocument = Unset }
    , Cmd.none
    )


handleDeleteDocument : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocument wrapMsg appState model =
    case model.documentToBeDeleted of
        Just document ->
            let
                newModel =
                    { model | deletingDocument = Loading }

                cmd =
                    Cmd.map wrapMsg <|
                        DocumentsApi.deleteDocument appState (Uuid.toString document.uuid) DeleteDocumentCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteDocumentCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocumentCompleted wrapMsg appState projectUuid model result =
    case result of
        Ok _ ->
            let
                ( documents, cmd ) =
                    Listing.update (listingUpdateConfig wrapMsg appState projectUuid) appState ListingMsgs.Reload model.documents
            in
            ( { model
                | deletingDocument = Success <| gettext "Document was successfully deleted." appState.locale
                , documents = documents
                , documentToBeDeleted = Nothing
              }
            , cmd
            )

        Err error ->
            ( { model | deletingDocument = ApiError.toActionResult appState (gettext "Document could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Document -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg projectUuid model =
    let
        ( documents, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState projectUuid) appState listingMsg model.documents
    in
    ( { model | documents = documents }
    , cmd
    )


handleShowHideSubmitDocument : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Maybe Document -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideSubmitDocument wrapMsg appState model mbDocument =
    let
        cmd =
            case mbDocument of
                Just document ->
                    Cmd.map wrapMsg <|
                        DocumentsApi.getSubmissionServices appState (Uuid.toString document.uuid) GetSubmissionServicesCompleted

                Nothing ->
                    Cmd.none
    in
    ( { model
        | documentToBeSubmitted = mbDocument
        , submittingDocument = Unset
        , submissionServices = Loading
        , selectedSubmissionServiceId = Nothing
      }
    , cmd
    )


handleGetSubmissionServicesCompleted : AppState -> Model -> Result ApiError (List SubmissionService) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetSubmissionServicesCompleted appState model result =
    let
        setResult value record =
            let
                selectedSubmissionServiceId =
                    value
                        |> ActionResult.map (Maybe.map .id << List.head)
                        |> ActionResult.withDefault Nothing
            in
            { record
                | submissionServices = value
                , selectedSubmissionServiceId = selectedSubmissionServiceId
            }
    in
    RequestHelpers.applyResult
        { setResult = setResult
        , defaultError = gettext "Unable to get submission services for the document." appState.locale
        , model = model
        , result = result
        , logoutMsg = Wizard.Msgs.logoutMsg
        , locale = appState.locale
        }


handleSelectSubmissionService : Model -> String -> ( Model, Cmd Wizard.Msgs.Msg )
handleSelectSubmissionService model id =
    ( { model | selectedSubmissionServiceId = Just id }, Cmd.none )


handleSubmitDocument : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmitDocument wrapMsg appState model =
    case ( model.documentToBeSubmitted, model.selectedSubmissionServiceId ) of
        ( Just document, Just serviceId ) ->
            ( { model | submittingDocument = Loading }
            , Cmd.map wrapMsg <|
                DocumentsApi.postSubmission appState serviceId (Uuid.toString document.uuid) SubmitDocumentCompleted
            )

        _ ->
            ( model, Cmd.none )


handleSubmitDocumentCompleted : AppState -> Model -> Result ApiError Submission -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmitDocumentCompleted appState model result =
    let
        updateSubmissions m =
            case result of
                Ok submission ->
                    addDocumentSubmission submission m

                _ ->
                    m
    in
    RequestHelpers.applyResult
        { setResult = \value record -> updateSubmissions { record | submittingDocument = value }
        , defaultError = gettext "Unable to submit the document." appState.locale
        , model = model
        , result = result
        , logoutMsg = Wizard.Msgs.logoutMsg
        , locale = appState.locale
        }



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Listing.UpdateConfig Document
listingUpdateConfig wrapMsg appState projectUuid =
    { getRequest = ProjectsApi.getDocuments appState projectUuid
    , getError = gettext "Unable to get documents." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.projectsDetailDocumentsWithFilters projectUuid
    }
