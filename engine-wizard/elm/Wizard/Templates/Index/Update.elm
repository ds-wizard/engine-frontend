module Wizard.Templates.Index.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Templates.Index.Models exposing (Model)
import Wizard.Templates.Index.Msgs exposing (Msg(..))
import Wizard.Templates.Routes exposing (Route(..))


fetchData : Cmd Msg
fetchData =
    Cmd.map ListingMsg Listing.fetchData


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ShowHideDeleteTemplate package ->
            ( { model | templateToBeDeleted = package, deletingTemplate = Unset }, Cmd.none )

        DeleteTemplate ->
            handleDeletePackage wrapMsg appState model

        DeleteTemplateCompleted result ->
            deletePackageCompleted appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model

        ExportTemplate template ->
            ( model, Ports.downloadFile (TemplatesApi.exportTemplateUrl template.id appState) )


handleDeletePackage : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeletePackage wrapMsg appState model =
    case model.templateToBeDeleted of
        Just template ->
            ( { model | deletingTemplate = Loading }
            , Cmd.map wrapMsg <|
                TemplatesApi.deleteTemplate template.organizationId template.templateId appState DeleteTemplateCompleted
            )

        Nothing ->
            ( model, Cmd.none )


deletePackageCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deletePackageCompleted appState model result =
    case result of
        Ok _ ->
            ( model
            , cmdNavigate appState (Listing.toRouteAfterDelete Routes.templatesIndexWithFilters model.templates)
            )

        Err error ->
            ( { model | deletingTemplate = ApiError.toActionResult appState (gettext "Document template could not be deleted." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )


handleListingMsg : (Msg -> Wizard.Msgs.Msg) -> AppState -> ListingMsgs.Msg Template -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleListingMsg wrapMsg appState listingMsg model =
    let
        ( templates, cmd ) =
            Listing.update (listingUpdateConfig wrapMsg appState) appState listingMsg model.templates
    in
    ( { model | templates = templates }
    , cmd
    )



-- Utils


listingUpdateConfig : (Msg -> Wizard.Msgs.Msg) -> AppState -> Listing.UpdateConfig Template
listingUpdateConfig wrapMsg appState =
    { getRequest = TemplatesApi.getTemplates
    , getError = gettext "Unable to get document templates." appState.locale
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.TemplatesRoute << IndexRoute
    }
