module Wizard.Documents.Index.Update exposing (..)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (applyResult, applyResultTransform, getResultCmd)
import Wizard.Common.Api.Documents as DocumentsApi
import Wizard.Common.Api.Questionnaires as QuestionnaireApi
import Wizard.Common.Api.Submissions as SubmissionsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing
import Wizard.Common.Setters exposing (setDocuments, setQuestionnaire)
import Wizard.Documents.Common.Document as Document exposing (Document)
import Wizard.Documents.Common.Submission exposing (Submission)
import Wizard.Documents.Common.SubmissionService exposing (SubmissionService)
import Wizard.Documents.Index.Models exposing (Model, updateStates)
import Wizard.Documents.Index.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    let
        questionnaireCmd =
            case model.questionnaireUuid of
                Just questionnaireUuid ->
                    QuestionnaireApi.getQuestionnaire questionnaireUuid appState GetQuestionnaireCompleted

                Nothing ->
                    Cmd.none
    in
    Cmd.batch
        [ DocumentsApi.getDocuments model.questionnaireUuid appState GetDocumentsCompleted
        , questionnaireCmd
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetDocumentsCompleted result ->
            handleGetDocumentsCompleted appState model result

        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        ShowHideDeleteDocument mbDocument ->
            handleShowHideDeleteDocument model mbDocument

        DeleteDocument ->
            handleDeleteDocument wrapMsg appState model

        DeleteDocumentCompleted result ->
            handleDeleteDocumentCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg listingMsg model

        RefreshDocuments ->
            handleRefreshDocuments wrapMsg appState model

        RefreshDocumentsCompleted result ->
            handleRefreshDocumentsCompleted model result

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



-- Handlers


handleGetDocumentsCompleted : AppState -> Model -> Result ApiError (List Document) -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetDocumentsCompleted appState model result =
    applyResultTransform
        { setResult = setDocuments
        , defaultError = lg "apiError.documents.getListError" appState
        , model = model
        , result = result
        , transform = Listing.modelFromList << List.sortWith Document.compare
        }


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    applyResult
        { setResult = setQuestionnaire << Just
        , defaultError = lg "apiError.documents.getListError" appState
        , model = model
        , result = result
        }


handleShowHideDeleteDocument : Model -> Maybe Document -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideDeleteDocument model mbDocument =
    ( { model | documentToBeDeleted = mbDocument, deletingDocument = Unset }
    , Cmd.none
    )


handleDeleteDocument : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocument wrapMsg appState model =
    case model.documentToBeDeleted of
        Just questionnaire ->
            let
                newModel =
                    { model | deletingDocument = Loading }

                cmd =
                    Cmd.map wrapMsg <|
                        DocumentsApi.deleteDocument questionnaire.uuid appState DeleteDocumentCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteDocumentCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocumentCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model
                | deletingDocument = Success <| lg "apiSuccess.documents.delete" appState
                , documents = Loading
                , documentToBeDeleted = Nothing
              }
            , Cmd.map wrapMsg <| fetchData appState model
            )

        Err error ->
            ( { model
                | deletingDocument = ApiError.toActionResult (lg "apiError.documents.deleteError" appState) error
              }
            , getResultCmd result
            )


handleListingMsg : Listing.Msg -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg listingMsg model =
    ( { model | documents = ActionResult.map (Listing.update listingMsg) model.documents }
    , Cmd.none
    )


handleRefreshDocuments : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleRefreshDocuments wrapMsg appState model =
    ( model
    , Cmd.map wrapMsg <|
        DocumentsApi.getDocuments model.questionnaireUuid appState RefreshDocumentsCompleted
    )


handleRefreshDocumentsCompleted : Model -> Result ApiError (List Document) -> ( Model, Cmd Wizard.Msgs.Msg )
handleRefreshDocumentsCompleted model result =
    case result of
        Ok documents ->
            ( updateStates model documents, Cmd.none )

        Err _ ->
            ( model, getResultCmd result )


handleShowHideSubmitDocument : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Maybe Document -> ( Model, Cmd Wizard.Msgs.Msg )
handleShowHideSubmitDocument wrapMsg appState model mbDocument =
    let
        cmd =
            case mbDocument of
                Just document ->
                    Cmd.map wrapMsg <|
                        DocumentsApi.getSubmissionServices document.uuid appState GetSubmissionServicesCompleted

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
    applyResult
        { setResult = \value record -> { record | submissionServices = value }
        , defaultError = lg "apiError.documents.getSubmissionServicesError" appState
        , model = model
        , result = result
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
                SubmissionsApi.postSubmission serviceId document.uuid appState SubmitDocumentCompleted
            )

        _ ->
            ( model, Cmd.none )


handleSubmitDocumentCompleted : AppState -> Model -> Result ApiError Submission -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmitDocumentCompleted appState model result =
    applyResult
        { setResult = \value record -> { record | submittingDocument = value }
        , defaultError = lg "apiError.submissions.postError" appState
        , model = model
        , result = result
        }
