module Wizard.Apps.Create.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Form
import Gettext exposing (gettext)
import Shared.Api.Apps as AppsApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Form exposing (setFormErrors)
import Wizard.Apps.Common.AppCreateForm as AppCreateForm
import Wizard.Apps.Create.Models exposing (Model)
import Wizard.Apps.Create.Msgs exposing (Msg(..))
import Wizard.Common.Api exposing (getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


update : AppState -> Msg -> (Msg -> Wizard.Msgs.Msg) -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update appState msg wrapMsg model =
    case msg of
        FormMsg formMsg ->
            handleForm formMsg wrapMsg appState model

        PostAppComplete result ->
            postAppCompleted appState model result


handleForm : Form.Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just appCreateForm ) ->
            let
                body =
                    AppCreateForm.encode appCreateForm

                cmd =
                    Cmd.map wrapMsg <|
                        AppsApi.postApp body appState PostAppComplete
            in
            ( { model | savingApp = Loading }, cmd )

        _ ->
            let
                newModel =
                    { model | form = Form.update AppCreateForm.validation formMsg model.form }
            in
            ( newModel, Cmd.none )


postAppCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
postAppCompleted appState model result =
    case result of
        Ok _ ->
            ( model, cmdNavigate appState Routes.appsIndex )

        Err error ->
            ( { model
                | savingApp = ApiError.toActionResult appState (gettext "App could not be created." appState.locale) error
                , form = setFormErrors appState error model.form
              }
            , getResultCmd Wizard.Msgs.logoutMsg result
            )
