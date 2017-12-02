module UserManagement.Edit.Update exposing (..)

import Auth.Models exposing (Session)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Jwt
import Msgs
import Requests exposing (toCmd)
import UserManagement.Edit.Models exposing (Model)
import UserManagement.Edit.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)
import UserManagement.Requests exposing (..)


getUserCmd : String -> Session -> Cmd Msgs.Msg
getUserCmd uuid session =
    getUser uuid session
        |> toCmd GetUserCompleted Msgs.UserManagementEditMsg


putUserCmd : Session -> UserEditForm -> String -> Cmd Msgs.Msg
putUserCmd session form uuid =
    form
        |> encodeUserEditForm uuid
        |> putUser uuid session
        |> toCmd PutUserCompleted Msgs.UserManagementEditMsg


putUserPasswordCmd : Session -> UserPasswordForm -> String -> Cmd Msgs.Msg
putUserPasswordCmd session form uuid =
    form
        |> encodeUserPasswordForm
        |> putUserPassword uuid session
        |> toCmd PutUserPasswordCompleted Msgs.UserManagementEditMsg


getUserCompleted : Model -> Result Jwt.JwtError User -> ( Model, Cmd Msgs.Msg )
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
    in
    ( newModel, Cmd.none )


putUserCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putUserCompleted model result =
    let
        editResult =
            case result of
                Ok user ->
                    Success "Profile was successfully updated"

                Err error ->
                    Error "Profile could not be saved."
    in
    ( { model | savingUser = editResult }, Cmd.none )


putUserPasswordCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putUserPasswordCompleted model result =
    let
        passwordResult =
            case result of
                Ok password ->
                    Success "Password was successfully changed"

                Err error ->
                    Error "Password could not be changed."
    in
    ( { model | savingPassword = passwordResult }, Cmd.none )


handleUserForm : Form.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleUserForm formMsg session model =
    case ( formMsg, Form.getOutput model.userForm ) of
        ( Form.Submit, Just userForm ) ->
            let
                cmd =
                    putUserCmd session userForm model.uuid
            in
            ( { model | savingUser = Loading }, cmd )

        _ ->
            let
                userForm =
                    Form.update userEditFormValidation formMsg model.userForm
            in
            ( { model | userForm = userForm }, Cmd.none )


handlePasswordForm : Form.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handlePasswordForm formMsg session model =
    case ( formMsg, Form.getOutput model.passwordForm ) of
        ( Form.Submit, Just passwordForm ) ->
            let
                cmd =
                    putUserPasswordCmd session passwordForm model.uuid
            in
            ( { model | savingPassword = Loading }, cmd )

        _ ->
            let
                passwordForm =
                    Form.update userPasswordFormValidation formMsg model.passwordForm
            in
            ( { model | passwordForm = passwordForm }, Cmd.none )


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetUserCompleted result ->
            getUserCompleted model result

        EditFormMsg formMsg ->
            handleUserForm formMsg session model

        PutUserCompleted result ->
            putUserCompleted model result

        PasswordFormMsg formMsg ->
            handlePasswordForm formMsg session model

        PutUserPasswordCompleted result ->
            putUserPasswordCompleted model result

        ChangeView view ->
            ( { model | currentView = view }, Cmd.none )
