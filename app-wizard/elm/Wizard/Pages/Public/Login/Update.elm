module Wizard.Pages.Public.Login.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.Token as Token
import Common.Data.Session as Session
import Common.Ports.LocalStorage as LocalStorage
import Gettext exposing (gettext)
import Json.Encode as E
import Json.Encode.Extra as E
import String.Extra as String
import Task.Extra as Task
import Wizard.Api.Models.BootstrapConfig.AdminConfig as Admin
import Wizard.Api.Models.TokenResponse as TokenResponse exposing (TokenResponse)
import Wizard.Api.OpenIdClients as OpenIdClientApi
import Wizard.Api.Tokens as TokensApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Auth.Msgs
import Wizard.Pages.Public.Login.Models exposing (Model)
import Wizard.Pages.Public.Login.Msgs exposing (Msg(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing exposing (cmdNavigate)


fetchData : AppState -> Maybe String -> Cmd Msg
fetchData appState mbOriginalUrl =
    if Session.isValid appState.currentTime appState.session then
        cmdNavigate appState Routes.appHome

    else if Admin.isEnabled appState.config.admin then
        Routing.cmdNavigateToLogin appState mbOriginalUrl

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
            , Cmd.map wrapMsg <| TokensApi.fetchToken appState body LoginCompleted
            )

        LoginCompleted result ->
            loginCompleted appState model result

        ExternalLoginOpenId openIdServiceConfig ->
            ( model
            , OpenIdClientApi.request appState openIdServiceConfig (wrapMsg << ExternalLoginOpenIdCompleted)
            )

        ExternalLoginOpenIdCompleted result ->
            case result of
                Ok openIdRequestResponse ->
                    ( model
                    , Cmd.batch
                        [ Navigation.load openIdRequestResponse.url
                        , saveOriginalUrlCmd model.originalUrl
                        , saveStateCmd openIdRequestResponse.state
                        ]
                    )

                Err error ->
                    ( { model | loggingIn = ApiError.toActionResult appState (gettext "External login failed." appState.locale) error }, Cmd.none )

        ShowAdminLogin ->
            ( { model | adminLoginVisible = True }, Cmd.none )


loginCompleted : AppState -> Model -> Result ApiError TokenResponse -> ( Model, Cmd Wizard.Msgs.Msg )
loginCompleted appState model result =
    case result of
        Ok tokenResponse ->
            case tokenResponse of
                TokenResponse.Token token expiresAt ->
                    ( model, Task.dispatch (Wizard.Msgs.AuthMsg <| Wizard.Pages.Auth.Msgs.GotToken (Token.create token expiresAt) model.originalUrl) )

                TokenResponse.CodeRequired ->
                    ( { model | codeRequired = True, loggingIn = Unset }, Cmd.none )

                TokenResponse.ConsentsRequired _ ->
                    ( model, Cmd.none )

                TokenResponse.IdentityLinked ->
                    ( model, Navigation.load (Routing.toUrl Routes.usersEditConnectedAccounts) )

                _ ->
                    ( { model | loggingIn = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }, Cmd.none )

        Err error ->
            ( { model | loggingIn = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )


saveOriginalUrlCmd : Maybe String -> Cmd msg
saveOriginalUrlCmd originalUrl =
    saveLocalStorageOptionalItem "wizard/originalUrl" originalUrl


saveStateCmd : String -> Cmd msg
saveStateCmd state =
    saveLocalStorageOptionalItem "wizard/state" (Just state)


saveLocalStorageOptionalItem : String -> Maybe String -> Cmd msg
saveLocalStorageOptionalItem key mbValue =
    case mbValue of
        Just value ->
            LocalStorage.setItem key (E.string value)

        Nothing ->
            Cmd.none
