module Wizard.DocumentTemplates.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.DocumentTemplates as DocumentTemplatesApi
import Shared.Data.DocumentTemplate exposing (DocumentTemplate)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Common.FileDownloader as FileDownloader
import Wizard.DocumentTemplates.Index.Models exposing (Model)
import Wizard.DocumentTemplates.Index.Msgs exposing (Msg(..))
import Wizard.DocumentTemplates.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


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
            deleteDocumentTemplateCompleted appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        UpdatePhase documentTemplate documentTemplatePhase ->
            let
                newDocumentTemplate =
                    { documentTemplate | phase = documentTemplatePhase }
            in
            ( model, DocumentTemplatesApi.putTemplate newDocumentTemplate appState (wrapMsg << UpdatePhaseCompleted) )

        UpdatePhaseCompleted result ->
            case result of
                Ok _ ->
                    ( model
                    , cmdNavigate appState (Listing.toRouteAfterDelete Routes.documentTemplatesIndexWithFilters model.documentTemplates)
                    )

                Err error ->
                    ( { model | deletingDocumentTemplate = ApiError.toActionResult appState (gettext "Document template could not be updated." appState.locale) error }
                    , getResultCmd Wizard.Msgs.logoutMsg result
                    )

        ExportDocumentTemplate documentTemplate ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.fetchFile appState (DocumentTemplatesApi.exportTemplateUrl documentTemplate.id appState)) )

        FileDownloaderMsg fileDownloaderMsg ->
            ( model, Cmd.map (wrapMsg << FileDownloaderMsg) (FileDownloader.update fileDownloaderMsg) )


handleDeleteDocumentTemplate : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteDocumentTemplate wrapMsg appState model =
    case model.documentTemplateToBeDeleted of
        Just template ->
            ( { model | deletingDocumentTemplate = Loading }
            , Cmd.map wrapMsg <|
                DocumentTemplatesApi.deleteTemplate template.organizationId template.templateId appState DeleteDocumentTemplateCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deleteDocumentTemplateCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteDocumentTemplateCompleted appState model result =
    case result of
        Ok _ ->
            ( model
            , cmdNavigate appState (Listing.toRouteAfterDelete Routes.documentTemplatesIndexWithFilters model.documentTemplates)
            )

        Err error ->
            ( { model | deletingDocumentTemplate = ApiError.toActionResult appState (gettext "Document template could not be deleted." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
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
    { getRequest = DocumentTemplatesApi.getTemplates
    , getError = gettext "Unable to get document templates." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.DocumentTemplatesRoute << IndexRoute
    }
