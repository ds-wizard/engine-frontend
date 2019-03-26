module Auth.Update exposing (update)

import Auth.Models exposing (initialSession, setToken, setUser)
import Auth.Msgs as AuthMsgs
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Models exposing (Model, setJwt, setSession)
import Msgs exposing (Msg)
import Ports
import Public.Login.Msgs
import Public.Msgs
import Routing exposing (Route(..), cmdNavigate, homeRoute)
import Users.Common.Models exposing (User)
import Utils exposing (dispatch)


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.Token token jwt ->
            let
                appState =
                    model.appState

                newModel =
                    model
                        |> setSession (setToken model.appState.session token)
                        |> setJwt (Just jwt)
            in
            ( newModel
            , UsersApi.getCurrentUser model.appState (AuthMsgs.GetCurrentUserCompleted >> Msgs.AuthMsg)
            )

        AuthMsgs.GetCurrentUserCompleted result ->
            getCurrentUserCompleted model result

        AuthMsgs.Logout ->
            logout model


getCurrentUserCompleted : Model -> Result ApiError User -> ( Model, Cmd Msg )
getCurrentUserCompleted model result =
    case result of
        Ok user ->
            let
                session =
                    setUser model.appState.session user
            in
            ( setSession session model
            , Cmd.batch
                [ Ports.storeSession <| Just session
                , cmdNavigate model.appState.key Welcome
                ]
            )

        Err error ->
            let
                msg =
                    getServerError error "Loading profile info failed"
                        |> Public.Login.Msgs.GetProfileInfoFailed
                        |> Public.Msgs.LoginMsg
                        |> Msgs.PublicMsg
            in
            ( model, dispatch msg )


logout : Model -> ( Model, Cmd Msg )
logout model =
    let
        cmd =
            Cmd.batch [ Ports.clearSession (), cmdNavigate model.appState.key homeRoute ]
    in
    ( setSession initialSession model, cmd )
