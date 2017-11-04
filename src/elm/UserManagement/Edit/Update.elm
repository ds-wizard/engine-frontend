module UserManagement.Edit.Update exposing (..)

import Auth.Models exposing (Session)
import Form exposing (Form)
import Jwt
import Msgs
import Routing exposing (Route(..), cmdNavigate)
import UserManagement.Edit.Models exposing (Model)
import UserManagement.Edit.Msgs exposing (Msg(..))
import UserManagement.Models exposing (..)
import UserManagement.Requests exposing (..)


getUserCmd : String -> Session -> Cmd Msgs.Msg
getUserCmd uuid session =
    Jwt.send GetUserCompleted (getUser uuid session) |> Cmd.map Msgs.UserManagementEditMsg


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


putUserCmd : Session -> UserEditForm -> String -> Cmd Msgs.Msg
putUserCmd session form uuid =
    form
        |> encodeUserEditForm uuid
        |> putUser uuid session
        |> Jwt.send PutUserCompleted
        |> Cmd.map Msgs.UserManagementEditMsg


putUserCompleted : Model -> Result Jwt.JwtError String -> ( Model, Cmd Msgs.Msg )
putUserCompleted model result =
    case result of
        Ok user ->
            ( model, cmdNavigate <| redirectRoute model )

        Err error ->
            ( { model | editError = "User could not be saved.", editSaving = False }, Cmd.none )


putUserPasswordCmd : Session -> UserPasswordForm -> String -> Cmd Msgs.Msg
putUserPasswordCmd session form uuid =
    form
        |> encodeUserPasswordForm
        |> putUserPassword uuid session
        |> Jwt.send PutPasswordCompleted
        |> Cmd.map Msgs.UserManagementEditMsg


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


update : Msg -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg session model =
    case msg of
        GetUserCompleted result ->
            getUserCompleted model result

        EditFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.editForm ) of
                ( Form.Submit, Just userEditForm ) ->
                    let
                        cmd =
                            putUserCmd session userEditForm model.uuid
                    in
                    ( { model | editSaving = True }, cmd )

                _ ->
                    ( { model | editForm = Form.update userEditFormValidation formMsg model.editForm }, Cmd.none )

        PutUserCompleted result ->
            putUserCompleted model result

        PasswordFormMsg formMsg ->
            case ( formMsg, Form.getOutput model.passwordForm ) of
                ( Form.Submit, Just passwordForm ) ->
                    let
                        cmd =
                            putUserPasswordCmd session passwordForm model.uuid
                    in
                    ( { model | passwordSaving = True }, cmd )

                _ ->
                    ( { model | passwordForm = Form.update userPasswordFormValidation formMsg model.passwordForm }, Cmd.none )

        PutPasswordCompleted result ->
            putUserPasswordCompleted model result
