module Wizard.Documents.Index.Update exposing (..)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Common.Api exposing (applyResultTransform, getResultCmd)
import Wizard.Common.Api.Documents as DocumentsApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing
import Wizard.Common.Setters exposing (setDocuments)
import Wizard.Documents.Common.Document as Document exposing (Document)
import Wizard.Documents.Index.Models exposing (Model, updateStates)
import Wizard.Documents.Index.Msgs exposing (Msg(..))
import Wizard.Msgs


fetchData : AppState -> Model -> Cmd Msg
fetchData appState model =
    DocumentsApi.getDocuments model.questionnaireUuid appState GetDocumentsCompleted


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GetDocumentsCompleted result ->
            handleGetDocumentsCompleted appState model result

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
