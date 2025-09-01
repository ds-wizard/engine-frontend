module Wizard.Settings.Registry.Update exposing
    ( fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form
import Gettext exposing (gettext)
import Shared.Data.ApiError as ApiError exposing (ApiError)
import Shared.Utils.Form as Form
import Shared.Utils.RequestHelpers as RequestHelpers
import Wizard.Api.Models.EditableConfig as EditableConfig
import Wizard.Api.Models.EditableConfig.EditableRegistryConfig as EditableRegistryConfig
import Wizard.Api.Registry as RegistryApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Settings.Common.Forms.RegistrySignupForm as RegistrySignupForm
import Wizard.Settings.Generic.Msgs as GenericMsgs
import Wizard.Settings.Generic.Update as GenericUpdate
import Wizard.Settings.Registry.Models exposing (Model)
import Wizard.Settings.Registry.Msgs exposing (Msg(..))


fetchData : AppState -> Cmd Msg
fetchData =
    Cmd.map GenericMsg << GenericUpdate.fetchData


update : (Msg -> Wizard.Msgs.Msg) -> Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update wrapMsg msg appState model =
    case msg of
        GenericMsg genericMsg ->
            handleGenericMsg wrapMsg genericMsg appState model

        ToggleRegistrySignup isOpen ->
            ( { model | registrySignupOpen = isOpen, registrySigningUp = Unset }, Cmd.none )

        FormMsg formMsg ->
            handleForm wrapMsg formMsg appState model

        PostSignupComplete result ->
            handlePostSignupComplete appState model result


handleGenericMsg : (Msg -> Wizard.Msgs.Msg) -> GenericMsgs.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleGenericMsg wrapMsg genericMsg appState model =
    let
        updateProps =
            { initForm = .registry >> EditableRegistryConfig.initForm
            , formToConfig = EditableConfig.updateRegistry
            , formValidation = EditableRegistryConfig.validation
            }

        ( genericModel, cmd ) =
            GenericUpdate.update updateProps (wrapMsg << GenericMsg) genericMsg appState model.genericModel

        registrySignupForm =
            case ( genericMsg, genericModel.config ) of
                ( GenericMsgs.GetConfigCompleted _, Success config ) ->
                    RegistrySignupForm.init appState config.organization

                _ ->
                    model.registrySignupForm
    in
    ( { model
        | genericModel = genericModel
        , registrySignupForm = registrySignupForm
      }
    , cmd
    )


handleForm : (Msg -> Wizard.Msgs.Msg) -> Form.Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
handleForm wrapMsg formMsg appState model =
    let
        registrySignupForm =
            Form.update (RegistrySignupForm.validation appState) formMsg model.registrySignupForm
    in
    case ( formMsg, Form.getOutput model.registrySignupForm ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    RegistrySignupForm.encode form

                cmd =
                    Cmd.map wrapMsg <|
                        RegistryApi.postSignup appState body PostSignupComplete
            in
            ( { model | registrySigningUp = Loading, registrySignupForm = registrySignupForm }
            , cmd
            )

        _ ->
            ( { model | registrySignupForm = registrySignupForm }
            , Cmd.none
            )


handlePostSignupComplete : AppState -> Model -> Result ApiError () -> ( Model, Cmd Wizard.Msgs.Msg )
handlePostSignupComplete appState model result =
    case result of
        Ok _ ->
            ( { model | registrySigningUp = Success (gettext "You have signed up for the Registry, you should receive a confirmation email soon." appState.locale) }
            , Cmd.none
            )

        Err error ->
            ( { model
                | registrySigningUp = ApiError.toActionResult appState (gettext "Sign up request failed." appState.locale) error
                , registrySignupForm = Form.setFormErrors appState error model.registrySignupForm
              }
            , RequestHelpers.getResultCmd Wizard.Msgs.logoutMsg result
            )
