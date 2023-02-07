module Wizard.DocumentTemplateEditors.Index.Update exposing (fetchData, update)

import ActionResult
import Gettext exposing (gettext)
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Data.DocumentTemplateDraft exposing (DocumentTemplateDraft)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.DocumentTemplateEditors.Index.Models exposing (Model)
import Wizard.DocumentTemplateEditors.Index.Msgs exposing (Msg(..))
import Wizard.DocumentTemplateEditors.Routes exposing (Route(..))
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


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
            deleteTemplateCompleted appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model


handleDeleteTemplate : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteTemplate wrapMsg appState model =
    case model.documentTemplateDraftToBeDeleted of
        Just template ->
            ( { model | deletingDocumentTemplateDraft = ActionResult.Loading }
            , Cmd.map wrapMsg <|
                DocumentTemplateDraftsApi.deleteDraft template.id appState DeleteDocumentTemplateDraftCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deleteTemplateCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteTemplateCompleted appState model result =
    case result of
        Ok _ ->
            ( model
            , cmdNavigate appState (Listing.toRouteAfterDelete Routes.documentTemplateEditorsIndexWithFilters model.documentTemplateDrafts)
            )

        Err error ->
            ( { model | deletingDocumentTemplateDraft = ApiError.toActionResult appState (gettext "Document template editor could not be deleted." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
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
    { getRequest = DocumentTemplateDraftsApi.getDrafts
    , getError = gettext "Unable to get document templates." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.DocumentTemplateEditorsRoute << IndexRoute
    }
