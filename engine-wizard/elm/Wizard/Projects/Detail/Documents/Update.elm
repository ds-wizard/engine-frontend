module Wizard.Projects.Detail.Documents.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Shared.Api.Documents as DocumentsApi
import Shared.Api.Questionnaires as QuestionnairesApi
import Shared.Api.Submissions as SubmissionsApi
import Shared.Data.Document exposing (Document)
import Shared.Data.Submission exposing (Submission)
import Shared.Data.SubmissionService exposing (SubmissionService)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Projects.Detail.Documents.Models exposing (Model)
import Wizard.Projects.Detail.Documents.Msgs exposing (Msg(..))
import Wizard.Projects.Detail.PlanDetailRoute as PlanDetailRoute
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState questionnaireUuid model =
    case msg of
        ShowHideDeleteDocument mbDocument ->
            handleShowHideDeleteDocument model mbDocument

        DeleteDocument ->
            handleDeleteDocument wrapMsg appState model

        DeleteDocumentCompleted result ->
            handleDeleteDocumentCompleted wrapMsg appState questionnaireUuid model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg questionnaireUuid model

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


handleDeleteDocumentCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocumentCompleted wrapMsg appState questionnaireUuid model result =
    case result of
        Ok _ ->
            let
                ( documents, cmd ) =
                    Listing.update (listingUpdateConfig wrapMsg appState questionnaireUuid) appState ListingMsgs.Reload model.documents
            in
            ( { model
                | deletingDocument = Success <| lg "apiSuccess.documents.delete" appState
                , documents = documents
                , documentToBeDeleted = Nothing
              }
            , cmd
            )

        Err error ->
            ( { model | deletingDocument = ApiError.toActionResult appState (lg "apiError.documents.deleteError" appState) error }
            , getResultCmd result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Document -> Uuid -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg questionnaireUuid model =
    let
        ( documents, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState questionnaireUuid) appState listingMsg model.documents
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
                SubmissionsApi.postSubmission serviceId (Uuid.toString document.uuid) appState SubmitDocumentCompleted
            )

        _ ->
            ( model, Cmd.none )


handleSubmitDocumentCompleted : AppState -> Model -> Result ApiError Submission -> ( Model, Cmd Wizard.Msgs.Msg )
handleSubmitDocumentCompleted appState model result =
    applyResult appState
        { setResult = \value record -> { record | submittingDocument = value }
        , defaultError = lg "apiError.submissions.postError" appState
        , model = model
        , result = result
        }



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Uuid -> Listing.UpdateConfig Document
listingUpdateConfig wrapMsg appState questionnaireUuid =
    { getRequest = QuestionnairesApi.getDocuments questionnaireUuid
    , getError = lg "apiError.documents.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.ProjectsRoute << DetailRoute questionnaireUuid << PlanDetailRoute.Documents
    }
