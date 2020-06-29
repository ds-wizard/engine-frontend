module Wizard.Templates.Index.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Templates as TemplatesApi
import Shared.Data.Template as Template
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (l, lg)
import Shared.Setters exposing (setTemplates)
import Wizard.Common.Api exposing (applyResultTransform, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.Listing as Listing
import Wizard.Msgs
import Wizard.Templates.Index.Models exposing (Model)
import Wizard.Templates.Index.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Templates.Index.Update"


fetchData : AppState -> Cmd Msg
fetchData appState =
    TemplatesApi.getTemplates appState GetTemplatesCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetTemplatesCompleted result ->
            applyResultTransform
                { setResult = setTemplates
                , defaultError = lg "apiError.templates.getListError" appState
                , model = model
                , result = result
                , transform = Listing.modelFromList << List.sortWith (Template.compare Nothing)
                }

        ShowHideDeleteTemplate package ->
            ( { model | templateToBeDeleted = package, deletingTemplate = Unset }, Cmd.none )

        DeleteTemplate ->
            handleDeletePackage wrapMsg appState model

        DeleteTemplateCompleted result ->
            deletePackageCompleted wrapMsg appState model result

        ListingMsg listingMsg ->
            ( { model | templates = ActionResult.map (Listing.update listingMsg) model.templates }
            , Cmd.none
            )


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
            ( { model
                | deletingTemplate = Success <| lg "apiSuccess.templates.delete" appState
                , templates = Loading
                , templateToBeDeleted = Nothing
              }
            , Cmd.map wrapMsg <| fetchData appState
            )

        Err error ->
            ( { model | deletingTemplate = ApiError.toActionResult (lg "apiError.templates.deleteError" appState) error }
            , getResultCmd result
            )
