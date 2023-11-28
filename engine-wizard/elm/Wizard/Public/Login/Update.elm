module Wizard.Public.Login.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Gettext exposing (gettext)
import Json.Encode as E
import Json.Encode.Extra as E
import Shared.Api.Auth as AuthApi
import Shared.Api.Tokens as TokensApi
import Shared.Auth.Session as Session
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.Token as Token
import Shared.Data.TokenResponse as TokenResponse exposing (TokenResponse)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Utils exposing (dispatch)
import String.Extra as String
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.Login.Models exposing (Model)
import Wizard.Public.Login.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing exposing (cmdNavigate)


fetchData : AppState -> Cmd msg
fetchData appState =
    if Session.exists appState.session && not (Session.expired appState.currentTime appState.session) then
        cmdNavigate appState Routes.appHome

    else if Admin.isEnabled appState.config.admin then
        case List.head appState.config.authentication.external.services of
            Just service ->
                Navigation.load (AuthApi.authRedirectUrl service appState)

            Nothing ->
                Cmd.none

    else
        Cmd.none


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Code code ->
            ( { model | code = code }, Cmd.none )

        DoLogin ->
            let
                body =
                    E.object
                        [ ( "email", E.string model.email )
                        , ( "password", E.string model.password )
                        , ( "code", E.maybe E.int (Maybe.andThen String.toInt (String.toMaybe model.code)) )
                        ]
            in
            ( { model | loggingIn = Loading }
            , Cmd.map wrapMsg <| TokensApi.fetchToken body appState LoginCompleted
            )

        LoginCompleted result ->
            loginCompleted appState model result

        ExternalLoginOpenId openIdServiceConfig ->
            let
                redirectCmd =
                    Navigation.load (AuthApi.authRedirectUrl openIdServiceConfig appState)
            in
            ( model, redirectCmd )


loginCompleted : AppState -> Model -> Result ApiError TokenResponse -> ( Model, Cmd Wizard.Msgs.Msg )
loginCompleted appState model result =
    case result of
        Ok tokenResponse ->
            case tokenResponse of
                TokenResponse.Token token expiresAt ->
                    ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken (Token.create token expiresAt) model.originalUrl) )

                TokenResponse.CodeRequired ->
                    ( { model | codeRequired = True, loggingIn = Unset }, Cmd.none )

                TokenResponse.ConsentsRequired _ ->
                    ( model, Cmd.none )

        Err error ->
            ( { model | loggingIn = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )
