module Wizard.Public.Auth.Update exposing (fetchData, update)

import ActionResult
import Gettext exposing (gettext)
import Shared.Api.Auth as AuthApi
import Shared.Data.Token as Token
import Shared.Data.TokenResponse as TokenResponse
import Shared.Error.ApiError as ApiError
import Shared.Utils exposing (dispatch)
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.Auth.Models exposing (Model)
import Wizard.Public.Auth.Msgs exposing (Msg(..))


fetchData : String -> Maybe String -> Maybe String -> Maybe String -> AppState -> Cmd Msg
fetchData id error code appState sessionState =
    AuthApi.getToken id error code appState sessionState AuthenticationCompleted


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        AuthenticationCompleted result ->
            case result of
                Ok tokenResponse ->
                    case tokenResponse of
                        TokenResponse.Token token expiresAt ->
                            ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken (Token.create token expiresAt) Nothing) )

                        TokenResponse.ConsentsRequired hash ->
                            ( { model | hash = Just hash }, Cmd.none )

                        TokenResponse.CodeRequired ->
                            ( { model | authenticating = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }, Cmd.none )

                Err error ->
                    ( { model | authenticating = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )

        CheckConsent value ->
            ( { model | consent = value }, Cmd.none )

        SubmitConsent ->
            case model.hash of
                Just hash ->
                    let
                        cmd =
                            Cmd.map wrapMsg (AuthApi.postConsents model.id hash model.sessionState appState SubmitConsentCompleted)
                    in
                    ( { model | submittingConsent = ActionResult.Loading }
                    , cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        SubmitConsentCompleted result ->
            case result of
                Ok tokenResponse ->
                    case tokenResponse of
                        TokenResponse.Token token expiresAt ->
                            ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken (Token.create token expiresAt) Nothing) )

                        _ ->
                            ( { model | submittingConsent = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }, Cmd.none )

                Err error ->
                    ( { model | submittingConsent = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )
