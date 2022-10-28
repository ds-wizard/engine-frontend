module Wizard.Auth.Update exposing (update)

import Gettext exposing (gettext)
import Shared.Api.Users as UsersApi
import Shared.Auth.Session as Session
import Shared.Data.User as User exposing (User)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Utils exposing (dispatch)
import Wizard.Auth.Msgs as AuthMsgs
import Wizard.Models exposing (Model, setSession)
import Wizard.Msgs exposing (Msg)
import Wizard.Ports as Ports
import Wizard.Public.Login.Msgs
import Wizard.Public.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate, cmdNavigateRaw)


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.GotToken token mbOriginalUrl ->
            let
                newModel =
                    setSession (Session.setToken model.appState.session token) model
            in
            ( newModel
            , UsersApi.getCurrentUser newModel.appState (AuthMsgs.GetCurrentUserCompleted mbOriginalUrl >> Wizard.Msgs.AuthMsg)
            )

        AuthMsgs.GetCurrentUserCompleted mbOriginalUrl result ->
            getCurrentUserCompleted model mbOriginalUrl result

        AuthMsgs.Logout ->
            logout model


getCurrentUserCompleted : Model -> Maybe String -> Result ApiError User -> ( Model, Cmd Msg )
getCurrentUserCompleted model mbOriginalUrl result =
    case result of
        Ok user ->
            let
                session =
                    Session.setUser model.appState.session (User.toUserInfo user)

                cmd =
                    case mbOriginalUrl of
                        Just originalUrl ->
                            cmdNavigateRaw model.appState originalUrl

                        Nothing ->
                            cmdNavigate model.appState Routes.DashboardRoute
            in
            ( setSession session model
            , Cmd.batch
                [ Ports.storeSession <| Session.encode session
                , cmd
                ]
            )

        Err error ->
            let
                msg =
                    ApiError.toActionResult model.appState (gettext "Loading the profile info failed." model.appState.locale) error
                        |> Wizard.Public.Login.Msgs.GetProfileInfoFailed
                        |> Wizard.Public.Msgs.LoginMsg
                        |> Wizard.Msgs.PublicMsg
            in
            ( model, dispatch msg )


logout : Model -> ( Model, Cmd Msg )
logout model =
    let
        cmd =
            Cmd.batch [ Ports.clearSession (), cmdNavigate model.appState Routes.publicHome ]
    in
    ( setSession Session.init model, cmd )
