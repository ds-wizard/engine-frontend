module Wizard.Pages.DocumentTemplates.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Components.FileDownloader as FileDownloader
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Wizard.Api.DocumentTemplates as DocumentTemplatesApi
import Wizard.Api.Models.DocumentTemplate exposing (DocumentTemplate)
import Wizard.Components.Listing.Msgs as ListingMsgs
import Wizard.Components.Listing.Update as Listing
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.DocumentTemplates.Index.Models exposing (Model)
import Wizard.Pages.DocumentTemplates.Index.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeleteDocumentTemplate template ->
            ( { model | documentTemplateToBeDeleted = template, deletingDocumentTemplate = Unset }, Cmd.none )

        DeleteDocumentTemplate ->
            handleDeleteDocumentTemplate wrapMsg appState model

        DeleteDocumentTemplateCompleted result ->
            deleteDocumentTemplateCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        UpdatePhase documentTemplate documentTemplatePhase ->
            let
                newDocumentTemplate =
                    { documentTemplate | phase = documentTemplatePhase }
            in
            ( model, DocumentTemplatesApi.putTemplate appState newDocumentTemplate (wrapMsg << UpdatePhaseCompleted) )

        UpdatePhaseCompleted result ->
            case result of
                Ok _ ->
                    ( model
                    , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
                    )

                Err error ->
                    ( { model | deletingDocumentTemplate = ApiError.toActionResult appState (gettext "Document template could not be updated." appState.locale) error }
                    , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
                    )

        ExportDocumentTemplate documentTemplate ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile (AppState.toServerInfo appState) (DocumentTemplatesApi.exportTemplateUrl appState documentTemplate.id)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeleteDocumentTemplate : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocumentTemplate wrapMsg appState model =
    case model.documentTemplateToBeDeleted of
        Just template ->
            ( { model | deletingDocumentTemplate = Loading }
            , Cmd.map wrapMsg <|
                DocumentTemplatesApi.deleteTemplate appState template.organizationId template.templateId DeleteDocumentTemplateCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deleteDocumentTemplateCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteDocumentTemplateCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | documentTemplateToBeDeleted = Nothing }
            , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
            )

        Err error ->
            ( { model | deletingDocumentTemplate = ApiError.toActionResult appState (gettext "Document template could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg DocumentTemplate -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( templates, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.documentTemplates
    in
    ( { model | documentTemplates = templates }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig DocumentTemplate
listingUpdateConfig wrapMsg appState =
    { getRequest = DocumentTemplatesApi.getTemplates appState
    , getError = gettext "Unable to get document templates." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.documentTemplatesIndexWithFilters
    }
