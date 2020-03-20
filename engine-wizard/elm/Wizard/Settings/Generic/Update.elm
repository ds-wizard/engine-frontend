module Wizard.Settings.Generic.Update exposing
    ( UpdateProps
    , fetchData
    , update
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Form.Validate exposing (Validation)
import Json.Encode as E
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api exposing (ToMsg, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Settings.Generic.Model exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))


type alias UpdateProps config form =
    { initForm : config -> Form CustomFormError form
    , getConfig : AppState -> ToMsg config (Msg config) -> Cmd (Msg config)
    , putConfig : E.Value -> AppState -> ToMsg () (Msg config) -> Cmd (Msg config)
    , locApiGetError : AppState -> String
    , locApiPutError : AppState -> String
    , encodeConfig : config -> E.Value
    , formToConfig : form -> config
    , formValidation : Validation CustomFormError form
    }


fetchData : UpdateProps config form -> AppState -> Cmd (Msg config)
fetchData props appState =
    props.getConfig appState GetConfigCompleted


update :
    UpdateProps config form
    -> (Msg config -> Wizard.Msgs.Msg)
    -> Msg config
    -> AppState
    -> Model config form
    -> ( Model config form, Cmd Wizard.Msgs.Msg )
update props wrapMsg msg appState model =
    case msg of
        GetConfigCompleted result ->
            handleGetConfigCompleted props appState model result

        PutConfigCompleted result ->
            handlePutConfigCompleted props appState model result

        FormMsg formMsg ->
            handleForm props formMsg wrapMsg appState model


handleGetConfigCompleted :
    UpdateProps config form
    -> AppState
    -> Model config form
    -> Result ApiError config
    -> ( Model config form, Cmd Wizard.Msgs.Msg )
handleGetConfigCompleted props appState model result =
    let
        newModel =
            case result of
                Ok config ->
                    { model | form = props.initForm config, config = Success config }

                Err error ->
                    { model | config = ApiError.toActionResult (props.locApiGetError appState) error }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePutConfigCompleted :
    UpdateProps config form
    -> AppState
    -> Model config form
    -> Result ApiError ()
    -> ( Model config form, Cmd Wizard.Msgs.Msg )
handlePutConfigCompleted props appState model result =
    let
        ( newResult, cmd ) =
            case result of
                Ok _ ->
                    ( Success ()
                    , Ports.refresh ()
                    )

                Err error ->
                    ( ApiError.toActionResult (props.locApiPutError appState) error
                    , getResultCmd result
                    )
    in
    ( { model | savingConfig = newResult }, Cmd.batch [ cmd, Ports.scrollToTop ".Settings__content" ] )


handleForm :
    UpdateProps config form
    -> Form.Msg
    -> (Msg config -> Wizard.Msgs.Msg)
    -> AppState
    -> Model config form
    -> ( Model config form, Cmd Wizard.Msgs.Msg )
handleForm props formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.form ) of
        ( Form.Submit, Just form ) ->
            let
                body =
                    props.encodeConfig <| props.formToConfig form

                cmd =
                    Cmd.map wrapMsg <|
                        props.putConfig body appState PutConfigCompleted
            in
            ( { model | savingConfig = Loading }, cmd )

        _ ->
            let
                form =
                    Form.update props.formValidation formMsg model.form
            in
            ( { model | form = form }, Cmd.none )
