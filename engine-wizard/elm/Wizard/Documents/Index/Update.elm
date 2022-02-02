module Wizard.Documents.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Api.Documents as DocumentsApi
import Shared.Api.Questionnaires as QuestionnaireApi
import Shared.Data.Document exposing (Document)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.Submission exposing (Submission)
import Shared.Data.SubmissionService exposing (SubmissionService)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setQuestionnaire)
import Uuid
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Documents.Index.Models exposing (Model, addDocumentSubmission)
import Wizard.Documents.Index.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


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
        [ Cmd.map ListingMsg Listing.fetchData
        , questionnaireCmd
        ]


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetQuestionnaireCompleted result ->
            handleGetQuestionnaireCompleted appState model result

        ShowHideDeleteDocument mbDocument ->
            handleShowHideDeleteDocument model mbDocument

        DeleteDocument ->
            handleDeleteDocument wrapMsg appState model

        DeleteDocumentCompleted result ->
            handleDeleteDocumentCompleted appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

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

        SetSubmissionErrorModal mbError ->
            ( { model | submissionErrorModal = mbError }, Cmd.none )


handleGetQuestionnaireCompleted : AppState -> Model -> Result ApiError QuestionnaireDetail -> ( Model, Cmd Wizard.Msgs.Msg )
handleGetQuestionnaireCompleted appState model result =
    applyResult appState
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
                        DocumentsApi.deleteDocument (Uuid.toString questionnaire.uuid) appState DeleteDocumentCompleted
            in
            ( newModel, cmd )

        _ ->
            ( model, Cmd.none )


handleDeleteDocumentCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocumentCompleted appState model result =
    case result of
        Ok _ ->
            ( model
            , cmdNavigate appState (Listing.toRouteAfterDelete Routes.documentsIndexWithFilters model.documents)
            )

        Err error ->
            ( { model | deletingDocument = ApiError.toActionResult appState (lg "apiError.documents.deleteError" appState) error }
            , getResultCmd result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Document -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( documents, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState model) appState listingMsg model.documents
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
                        DocumentsApi.getSubmissionServices (Uuid.toString document.uuid) appState GetSubmissionServicesCompleted

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
    applyResult appState
        { setResult = setResult
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
                DocumentsApi.postSubmission serviceId (Uuid.toString document.uuid) appState SubmitDocumentCompleted
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
    applyResult appState
        { setResult = \value record -> updateSubmissions { record | submittingDocument = value }
        , defaultError = lg "apiError.submissions.postError" appState
        , model = model
        , result = result
        }



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Listing.UpdateConfig Document
listingUpdateConfig wrapMsg appState model =
    { getRequest = DocumentsApi.getDocuments model.questionnaireUuid
    , getError = lg "apiError.documents.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.DocumentsRoute << IndexRoute model.questionnaireUuid
    }
