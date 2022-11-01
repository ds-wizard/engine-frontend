module Wizard.Templates.Detail.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Templates as TemplatesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Setters exposing (setTemplate)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Templates.Detail.Models exposing (Model)
import Wizard.Templates.Detail.Msgs exposing (Msg(..))


fetchData : String -> AppState -> Cmd Msg
fetchData templateId appState =
    TemplatesApi.getTemplate templateId appState GetTemplateCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetTemplateCompleted result ->
            applyResult appState
                { setResult = setTemplate
                , defaultError = gettext "Unable to get the document template." appState.locale
                , model = model
                , result = result
                , logoutMsg = Wizard.Msgs.logoutMsg
                }

        ShowDeleteDialog visible ->
            ( { model | showDeleteDialog = visible, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            deleteVersionCompleted appState model result

        ExportTemplate template ->
            ( model, Ports.downloadFile (TemplatesApi.exportTemplateUrl template.id appState) )


handleDeleteVersion : (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleDeleteVersion wrapMsg appState model =
    case model.template of
        Success template ->
            ( { model | deletingVersion = Loading }
            , Cmd.map wrapMsg <| TemplatesApi.deleteTemplateVersion template.id appState DeleteVersionCompleted
            )

        _ ->
            ( model, Cmd.none )


deleteVersionCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
deleteVersionCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.templatesIndex )

        Err error ->
            ( { model | deletingVersion = ApiError.toActionResult appState (gettext "Document template could not be deleted." appState.locale) error }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
