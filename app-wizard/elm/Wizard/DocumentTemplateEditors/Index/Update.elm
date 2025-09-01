module Wizard.DocumentTemplateEditors.Index.Update exposing (fetchData, update)

import ActionResult
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.RequestHelpers as RequestHelpers
import Task.Extra as Task
import Wizard.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Wizard.Api.Models.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.DocumentTemplateEditors.Index.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))
import Wizard.Msgs
import Wizard.Routes as Routes


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeleteDocumentTemplateDraft template ->
            ( { model | documentTemplateDraftToBeDeleted = template, deletingDocumentTemplateDraft = ActionResult.Unset }, Cmd.none )

        DeleteDocumentTemplateDraft ->
            handleDeleteTemplate wrapMsg appState model

        DeleteDocumentTemplateDraftCompleted result ->
            deleteTemplateCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model


handleDeleteTemplate : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteTemplate wrapMsg appState model =
    case model.documentTemplateDraftToBeDeleted of
        Just template ->
            ( { model | deletingDocumentTemplateDraft = ActionResult.Loading }
            , Cmd.map wrapMsg <|
                DocumentTemplateDraftsApi.deleteDraft appState template.id DeleteDocumentTemplateDraftCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deleteTemplateCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteTemplateCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            ( { model | documentTemplateDraftToBeDeleted = Nothing }
            , Task.dispatch (wrapMsg (ListingMsg ListingMsgs.OnAfterDelete))
            )

        Err error ->
            ( { model | deletingDocumentTemplateDraft = ApiError.toActionResult appState (gettext "Document template editor could not be deleted." appState.locale) error }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg DocumentTemplateDraft -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( templates, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.documentTemplateDrafts
    in
    ( { model | documentTemplateDrafts = templates }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig DocumentTemplateDraft
listingUpdateConfig wrapMsg appState =
    { getRequest = DocumentTemplateDraftsApi.getDrafts appState
    , getError = gettext "Unable to get document templates." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.documentTemplateEditorsIndexWithFilters
    }
