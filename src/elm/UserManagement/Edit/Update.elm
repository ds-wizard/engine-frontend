module UserManagement.Edit.Update exposing (..)

import Auth.Models exposing (Session)
import Form exposing (Form)
import Jwt
import Msgs
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
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
                        editForm =
                            initUserEditForm user
                    in
                    { model | editForm = editForm, loading = False }

                Err error ->
                    { model | loadingError = "Unable to get user profile.", loading = False }
    in
    ( newModel, Cmd.none )


putUserCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putUserCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate <| redirectRoute model )

        Err error ->
            ( { model | editError = "User could not be saved.", editSaving = False }, Cmd.none )


putUserPasswordCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putUserPasswordCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate <| redirectRoute model )

        Err error ->
            ( { model | passwordError = "Password could not be changed.", passwordSaving = False }, Cmd.none )


redirectRoute : Model -> Route
redirectRoute model =
    if model.uuid == "current" then
        Index
    else
        UserManagement


handleEditForm : Form.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleEditForm formMsg session model =
    case ( formMsg, Form.getOutput model.editForm ) of
        ( Form.Submit, Just userEditForm ) ->
            let
                cmd =
                    putUserCmd session userEditForm model.uuid
            in
            ( { model | editSaving = True }, cmd )

        _ ->
            let
                editForm =
                    Form.update userEditFormValidation formMsg model.editForm
            in
            ( { model | editForm = editForm }, Cmd.none )


handlePasswordForm : Form.Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handlePasswordForm formMsg session model =
    case ( formMsg, Form.getOutput model.passwordForm ) of
        ( Form.Submit, Just passwordForm ) ->
            let
                cmd =
                    putUserPasswordCmd session passwordForm model.uuid
            in
            ( { model | passwordSaving = True }, cmd )

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
            handleEditForm formMsg session model

        PutUserCompleted result ->
            putUserCompleted model result

        PasswordFormMsg formMsg ->
            handlePasswordForm formMsg session model

        PutUserPasswordCompleted result ->
            putUserPasswordCompleted model result
