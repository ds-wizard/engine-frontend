module Wizard.Auth.Update exposing (update)

import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Auth.Msgs as AuthMsgs
import Wizard.Common.Api.Users as UsersApi
import Wizard.Common.Session as Session
import Wizard.Models exposing (Model, setJwt, setSession)
import Wizard.Msgs exposing (Msg)
import Wizard.Ports as Ports
import Wizard.Public.Login.Msgs
import Wizard.Public.Msgs
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate, cmdNavigateRaw, homeRoute)
import Wizard.Users.Common.User exposing (User)
import Wizard.Utils exposing (dispatch)


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.Token token jwt mbOriginalUrl ->
            let
                newModel =
                    model
                        |> setSession (Session.setToken model.appState.session token)
                        |> setJwt (Just jwt)
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
                    Session.setUser model.appState.session user

                cmd =
                    case mbOriginalUrl of
                        Just originalUrl ->
                            cmdNavigateRaw model.appState originalUrl

                        Nothing ->
                            cmdNavigate model.appState Routes.DashboardRoute
            in
            ( setSession session model
            , Cmd.batch
                [ Ports.storeSession <| Just session
                , cmd
                ]
            )

        Err error ->
            let
                msg =
                    ApiError.toActionResult (lg "apiError.users.current.getError" model.appState) error
                        |> Wizard.Public.Login.Msgs.GetProfileInfoFailed
                        |> Wizard.Public.Msgs.LoginMsg
                        |> Wizard.Msgs.PublicMsg
            in
            ( model, dispatch msg )


logout : Model -> ( Model, Cmd Msg )
logout model =
    let
        cmd =
            Cmd.batch [ Ports.clearSession (), cmdNavigate model.appState homeRoute ]
    in
    ( setSession Session.init model, cmd )
