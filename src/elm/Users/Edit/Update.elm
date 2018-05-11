module Users.Edit.Update exposing (fetchData, update)

import Auth.Models exposing (Session)
import Common.Models exposing (getServerErrorJwt)
import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import Jwt
import Msgs
import Users.Common.Models exposing (..)
import Users.Edit.Models exposing (..)
import Users.Edit.Msgs exposing (Msg(..))
import Users.Requests exposing (..)


fetchData : (Msg -> Msgs.Msg) -> Session -> String -> Cmd Msgs.Msg
fetchData wrapMsg session uuid =
    getUserCmd session uuid |> Cmd.map wrapMsg


update : Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg session model =
    case msg of
        ChangeView view ->
            ( { model | currentView = view }, Cmd.none )

        EditFormMsg formMsg ->
            handleUserForm formMsg wrapMsg session model

        GetUserCompleted result ->
            getUserCompleted model result

        PasswordFormMsg formMsg ->
            handlePasswordForm formMsg wrapMsg session model

        PutUserCompleted result ->
            putUserCompleted model result

        PutUserPasswordCompleted result ->
            putUserPasswordCompleted model result


getUserCmd : Session -> String -> Cmd Msg
getUserCmd session uuid =
    getUser uuid session |> Jwt.send GetUserCompleted


handleUserForm : Form.Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handleUserForm formMsg wrapMsg session model =
    case ( formMsg, Form.getOutput model.userForm ) of
        ( Form.Submit, Just userForm ) ->
            let
                cmd =
                    putUserCmd session userForm model.uuid |> Cmd.map wrapMsg
            in
            ( { model | savingUser = Loading }, cmd )

        _ ->
            let
                userForm =
                    Form.update userEditFormValidation formMsg model.userForm
            in
            ( { model | userForm = userForm }, Cmd.none )


putUserCmd : Session -> UserEditForm -> String -> Cmd Msg
putUserCmd session form uuid =
    form
        |> encodeUserEditForm uuid
        |> putUser uuid session
        |> Jwt.send PutUserCompleted


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


handlePasswordForm : Form.Msg -> (Msg -> Msgs.Msg) -> Session -> Model -> ( Model, Cmd Msgs.Msg )
handlePasswordForm formMsg wrapMsg session model =
    case ( formMsg, Form.getOutput model.passwordForm ) of
        ( Form.Submit, Just passwordForm ) ->
            let
                cmd =
                    putUserPasswordCmd session passwordForm model.uuid |> Cmd.map wrapMsg
            in
            ( { model | savingPassword = Loading }, cmd )

        _ ->
            let
                passwordForm =
                    Form.update userPasswordFormValidation formMsg model.passwordForm
            in
            ( { model | passwordForm = passwordForm }, Cmd.none )


putUserPasswordCmd : Session -> UserPasswordForm -> String -> Cmd Msg
putUserPasswordCmd session form uuid =
    form
        |> encodeUserPasswordForm
        |> putUserPassword uuid session
        |> Jwt.send PutUserPasswordCompleted


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
                    getServerErrorJwt error "Password could not be changed."
    in
    ( { model | savingPassword = passwordResult }, Cmd.none )
