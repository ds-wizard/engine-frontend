module Users.Edit.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Form exposing (Form)
import Msgs
import Result exposing (Result)
import Users.Common.Models exposing (..)
import Users.Edit.Models exposing (..)
import Users.Edit.Msgs exposing (Msg(..))


fetchData : (Msg -> Msgs.Msg) -> AppState -> String -> Cmd Msgs.Msg
fetchData wrapMsg appState uuid =
    Cmd.map wrapMsg <|
        UsersApi.getUser uuid appState GetUserCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangeView view ->
            ( { model | currentView = view }, Cmd.none )

        EditFormMsg formMsg ->
            handleUserForm formMsg wrapMsg appState model

        GetUserCompleted result ->
            getUserCompleted model result

        PasswordFormMsg formMsg ->
            handlePasswordForm formMsg wrapMsg appState model

        PutUserCompleted result ->
            putUserCompleted model result

        PutUserPasswordCompleted result ->
            putUserPasswordCompleted model result


handleUserForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleUserForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.userForm ) of
        ( Form.Submit, Just userForm ) ->
            let
                body =
                    encodeUserEditForm model.uuid userForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.putUser model.uuid body appState PutUserCompleted
            in
            ( { model | savingUser = Loading }, cmd )

        _ ->
            let
                userForm =
                    Form.update userEditFormValidation formMsg model.userForm
            in
            ( { model | userForm = userForm }, Cmd.none )


getUserCompleted : Model -> Result ApiError User -> ( Model, Cmd Msgs.Msg )
getUserCompleted model result =
    let
        newModel =
            case result of
                Ok user ->
                    let
                        userForm =
                            initUserEditForm user
                    in
                    { model | userForm = userForm, user = Success user }

                Err error ->
                    { model | user = Error "Unable to get user profile." }

        cmd =
            getResultCmd result
    in
    ( newModel, cmd )


handlePasswordForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handlePasswordForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.passwordForm ) of
        ( Form.Submit, Just passwordForm ) ->
            let
                body =
                    encodeUserPasswordForm passwordForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.putUserPassword model.uuid body appState PutUserPasswordCompleted
            in
            ( { model | savingPassword = Loading }, cmd )

        _ ->
            let
                passwordForm =
                    Form.update userPasswordFormValidation formMsg model.passwordForm
            in
            ( { model | passwordForm = passwordForm }, Cmd.none )


putUserCompleted : Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
putUserCompleted model result =
    let
        editResult =
            case result of
                Ok _ ->
                    Success "Profile was successfully updated"

                Err error ->
                    Error "Profile could not be saved."

        cmd =
            getResultCmd result
    in
    ( { model | savingUser = editResult }, cmd )


putUserPasswordCompleted : Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
putUserPasswordCompleted model result =
    let
        passwordResult =
            case result of
                Ok _ ->
                    Success "Password was successfully changed"

                Err error ->
                    getServerError error "Password could not be changed."

        cmd =
            getResultCmd result
    in
    ( { model | savingPassword = passwordResult }, cmd )
