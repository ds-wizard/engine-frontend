module Wizard.Templates.Detail.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Shared.Api.Templates as TemplatesApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Setters exposing (setTemplate)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)
import Wizard.Templates.Detail.Models exposing (..)
import Wizard.Templates.Detail.Msgs exposing (Msg(..))
import Wizard.Templates.Routes exposing (Route(..))


fetchData : String -> AppState -> Cmd Msg
fetchData templateId appState =
    TemplatesApi.getTemplate templateId appState GetTemplateCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        GetTemplateCompleted result ->
            applyResult appState
                { setResult = setTemplate
                , defaultError = lg "apiError.templates.getError" appState
                , model = model
                , result = result
                }

        ShowDeleteDialog visible ->
            ( { model | showDeleteDialog = visible, deletingVersion = Unset }, Cmd.none )

        DeleteVersion ->
            handleDeleteVersion wrapMsg appState model

        DeleteVersionCompleted result ->
            deleteVersionCompleted appState model result


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
            ( { model | deletingVersion = ApiError.toActionResult appState (lg "apiError.templates.deleteError" appState) error }
            , getResultCmd result
            )
