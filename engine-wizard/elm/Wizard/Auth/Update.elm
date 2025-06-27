module Wizard.Auth.Update exposing (update)

import Browser.Navigation as Navigation
import Shared.Auth.Session as Session
import Wizard.Api.Tokens as TokensApi
import Wizard.Auth.Msgs as AuthMsgs
import Wizard.Models exposing (Model, setSession)
import Wizard.Msgs exposing (Msg)
import Wizard.Ports as Ports
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.GotToken token mbOriginalUrl ->
            let
                newModel =
                    setSession (Session.setToken model.appState.session token) model

                redirectUrl =
                    case mbOriginalUrl of
                        Just originalUrl ->
                            originalUrl

                        Nothing ->
                            Routing.toUrl model.appState Routes.DashboardRoute
            in
            ( newModel
            , Cmd.batch
                [ Ports.storeSession (Session.encode newModel.appState.session)
                , Navigation.load redirectUrl
                ]
            )

        AuthMsgs.Logout ->
            logout model

        AuthMsgs.LogoutTo route ->
            logoutTo route model

        AuthMsgs.LogoutDone ->
            ( model, Cmd.none )


logout : Model -> ( Model, Cmd Msg )
logout =
    logoutTo Routes.publicLogoutSuccessful


logoutTo : Routes.Route -> Model -> ( Model, Cmd Msg )
logoutTo route model =
    let
        cmd =
            Cmd.batch
                [ Ports.clearSession ()
                , TokensApi.deleteCurrentToken model.appState (Wizard.Msgs.AuthMsg << always AuthMsgs.LogoutDone)
                , cmdNavigate model.appState route
                ]
    in
    ( setSession (Session.init model.appState.apiUrl) model, cmd )
