module Users.Edit.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Api exposing (getResultCmd)
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Form exposing (setFormErrors)
import Common.Locale exposing (lg)
import Form exposing (Form)
import Msgs
import Result exposing (Result)
import Users.Common.User exposing (User)
import Users.Common.UserEditForm as UserEditForm
import Users.Common.UserPasswordForm as UserPasswordForm
import Users.Edit.Models exposing (..)
import Users.Edit.Msgs exposing (Msg(..))


fetchData : AppState -> String -> Cmd Msg
fetchData appState uuid =
    UsersApi.getUser uuid appState GetUserCompleted


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        ChangeView view ->
            ( { model | currentView = view }, Cmd.none )

        EditFormMsg formMsg ->
            handleUserForm formMsg wrapMsg appState model

        GetUserCompleted result ->
            getUserCompleted appState model result

        PasswordFormMsg formMsg ->
            handlePasswordForm formMsg wrapMsg appState model

        PutUserCompleted result ->
            putUserCompleted appState model result

        PutUserPasswordCompleted result ->
            putUserPasswordCompleted appState model result


handleUserForm : Form.Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
handleUserForm formMsg wrapMsg appState model =
    case ( formMsg, Form.getOutput model.userForm ) of
        ( Form.Submit, Just userForm ) ->
            let
                body =
                    UserEditForm.encode model.uuid userForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.putUser model.uuid body appState PutUserCompleted
            in
            ( { model | savingUser = Loading }, cmd )

        _ ->
            let
                userForm =
                    Form.update UserEditForm.validation formMsg model.userForm
            in
            ( { model | userForm = userForm }, Cmd.none )


getUserCompleted : AppState -> Model -> Result ApiError User -> ( Model, Cmd Msgs.Msg )
getUserCompleted appState model result =
    let
        newModel =
            case result of
                Ok user ->
                    let
                        userForm =
                            UserEditForm.init user
                    in
                    { model | userForm = userForm, user = Success user }

                Err _ ->
                    { model | user = Error <| lg "apiError.users.getError" appState }

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
                    UserPasswordForm.encode passwordForm

                cmd =
                    Cmd.map wrapMsg <|
                        UsersApi.putUserPassword model.uuid body appState PutUserPasswordCompleted
            in
            ( { model | savingPassword = Loading }, cmd )

        _ ->
            let
                passwordForm =
                    Form.update UserPasswordForm.validation formMsg model.passwordForm
            in
            ( { model | passwordForm = passwordForm }, Cmd.none )


putUserCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
putUserCompleted appState model result =
    case result of
        Ok _ ->
            ( { model | savingUser = Success <| lg "apiSuccess.users.put" appState }, Cmd.none )

        Err err ->
            ( { model
                | savingUser = getServerError err <| lg "apiError.users.putError" appState
                , userForm = setFormErrors err model.userForm
              }
            , getResultCmd result
            )


putUserPasswordCompleted : AppState -> Model -> Result ApiError () -> ( Model, Cmd Msgs.Msg )
putUserPasswordCompleted appState model result =
    let
        passwordResult =
            case result of
                Ok _ ->
                    Success <| lg "apiSuccess.users.password.put" appState

                Err error ->
                    getServerError error <| lg "apiError.users.password.putError" appState

        cmd =
            getResultCmd result
    in
    ( { model | savingPassword = passwordResult }, cmd )
