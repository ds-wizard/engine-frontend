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
            , UsersApi.getCurrentUser newModel.appState (Wizard.Msgs.AuthMsg << AuthMsgs.GetCurrentUserCompleted mbOriginalUrl)
            )

        AuthMsgs.GetCurrentUserCompleted mbOriginalUrl result ->
            getCurrentUserCompleted model mbOriginalUrl result

        AuthMsgs.Logout ->
            logout model

        AuthMsgs.LogoutTo route ->
            logoutTo route model

        AuthMsgs.LogoutDone ->
            ( model, Cmd.none )

        AuthMsgs.UpdateUser user ->
            let
                session =
                    Session.setUser model.appState.session (User.toUserInfo user)
            in
            ( setSession session model
            , Ports.storeSession (Session.encode session)
            )


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
                [ Ports.storeSession (Session.encode session)
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
logout =
    logoutTo Routes.publicHome


logoutTo : Routes.Route -> Model -> ( Model, Cmd Msg )
logoutTo route model =
    let
        cmd =
            Cmd.batch
                [ Ports.clearSession ()
                , UsersApi.deleteToken model.appState (Wizard.Msgs.AuthMsg << always AuthMsgs.LogoutDone)
                , cmdNavigate model.appState route
                ]
    in
    ( setSession Session.init model, cmd )
