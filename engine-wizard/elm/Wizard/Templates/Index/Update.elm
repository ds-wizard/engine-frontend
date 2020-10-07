module Wizard.Templates.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Template exposing (Template)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg)
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing.Msgs as ListingMsgs
import Wizard.Common.Components.Listing.Update as Listing
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Templates.Index.Models exposing (Model)
import Wizard.Templates.Index.Msgs exposing (Msg(..))
import Wizard.Templates.Routes exposing (Route(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Templates.Index.Update"


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
            deletePackageCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            handleListingMsg wrapMsg appState listingMsg model


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


deletePackageCompleted : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deletePackageCompleted wrapMsg appState model result =
    case result of
        Ok _ ->
            let
                ( templates, cmd ) =
                    Listing.update (listingUpdateConfig wrapMsg appState) appState ListingMsgs.Reload model.templates
            in
            ( { model
                | deletingTemplate = Success <| lg "apiSuccess.templates.delete" appState
                , templates = templates
                , templateToBeDeleted = Nothing
              }
            , cmd
            )

        Err error ->
            ( { model | deletingTemplate = ApiError.toActionResult (lg "apiError.templates.deleteError" appState) error }
            , getResultCmd result
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
    { getRequest = TemplatesApi.getTemplatesPaginated
    , getError = lg "apiError.templates.getListError" appState
    , wrapMsg = wrapMsg << ListingMsg
    , toRoute = Routes.TemplatesRoute << IndexRoute
    }
