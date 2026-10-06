module Wizard.Pages.Auth.Update exposing (update)

import Browser.Navigation as Navigation
import Common.Api.Models.BuildInfo as BuildInfo
import Common.Components.NewsModal as NewsModal
import Common.Data.Session as Session
import Common.Ports.Session as Session
import Maybe.Extra as Maybe
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Api.Tokens as TokensApi
import Wizard.Data.AppState as AppState
import Wizard.Models exposing (Model, setSession)
import Wizard.Msgs exposing (Msg)
import Wizard.Pages.Auth.Msgs as AuthMsgs
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)
import Wizard.Utils.Feature as Feature


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.GotToken token mbOriginalUrl ->
            let
                newModel =
                    setSession (Session.setToken token model.appState.session) model

                redirectUrl =
                    case mbOriginalUrl of
                        Just originalUrl ->
                            if String.startsWith "/wizard/open-id" originalUrl then
                                Routing.toUrl Routes.DashboardRoute

                            else
                                originalUrl

                        Nothing ->
                            Routing.toUrl Routes.DashboardRoute

                ( newsModalModel, newsModalCmd ) =
                    if Feature.newsModal newModel.appState then
                        NewsModal.init newModel.appState.newsUrl BuildInfo.client.version (AppState.userPermissions newModel.appState)

                    else
                        ( NewsModal.initialModel, Cmd.none )
            in
            ( { newModel | newsModalModel = newsModalModel }
            , Cmd.batch
                [ Session.storeSession (Session.encode newModel.appState.session)
                , Navigation.load redirectUrl
                , Cmd.map Wizard.Msgs.NewsModalMsg newsModalCmd
                ]
            )

        AuthMsgs.Logout ->
            if Admin.isEnabled model.appState.config.admin then
                logoutAndLoad (Routing.loginUrl model.appState Nothing) model

            else
                logoutAndNavigate (cmdNavigate model.appState Routes.publicLogoutSuccessful) model

        AuthMsgs.LogoutToLogin mbOriginalUrl ->
            if Admin.isEnabled model.appState.config.admin then
                logoutAndLoad (Routing.loginUrl model.appState mbOriginalUrl) model

            else
                logoutAndNavigate (Routing.cmdNavigateToLogin model.appState mbOriginalUrl) model

        AuthMsgs.LogoutDone mbRedirectUrl ->
            ( model, Maybe.unwrap Cmd.none Navigation.load mbRedirectUrl )


{-| Log out and navigate within the Wizard client. The token is revoked in the background, the
navigation does not interrupt the request.
-}
logoutAndNavigate : Cmd Msg -> Model -> ( Model, Cmd Msg )
logoutAndNavigate navigateCmd model =
    logoutWith Nothing navigateCmd model


{-| Log out and leave the Wizard client. Loading another page cancels pending requests, so the
redirect has to wait until the token is revoked.
-}
logoutAndLoad : String -> Model -> ( Model, Cmd Msg )
logoutAndLoad redirectUrl model =
    logoutWith (Just redirectUrl) Cmd.none model


logoutWith : Maybe String -> Cmd Msg -> Model -> ( Model, Cmd Msg )
logoutWith mbRedirectUrl navigateCmd model =
    let
        cmd =
            Cmd.batch
                [ Session.clearSession ()
                , TokensApi.deleteCurrentToken model.appState (Wizard.Msgs.AuthMsg << always (AuthMsgs.LogoutDone mbRedirectUrl))
                , navigateCmd
                ]
    in
    ( setSession (Session.init model.appState.session.apiUrlBase) model, cmd )
