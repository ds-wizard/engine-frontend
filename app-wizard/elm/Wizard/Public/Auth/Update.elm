module Wizard.Public.Auth.Update exposing (fetchData, update)

import ActionResult
import Gettext exposing (gettext)
import Json.Decode as D
import Shared.Data.ApiError as ApiError
import Shared.Data.Token as Token
import Task.Extra as Task
import Wizard.Api.Auth as AuthApi
import Wizard.Api.Models.TokenResponse as TokenResponse
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.LocalStorageData as LocalStorageData
import Wizard.Msgs
import Wizard.Ports as Ports
import Wizard.Public.Auth.Models exposing (Model)
import Wizard.Public.Auth.Msgs exposing (Msg(..))


fetchData : String -> Maybe String -> Maybe String -> Maybe String -> AppState -> Cmd Msg
fetchData id mbError mbCode mbSessionState appState =
    Cmd.batch
        [ AuthApi.getToken appState id mbError mbCode mbSessionState AuthenticationCompleted
        , Ports.localStorageGetAndRemove "wizard/originalUrl"
        ]


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    let
        dispatchToken newModel =
            case ActionResult.combine newModel.token newModel.originalUrl of
                ActionResult.Success ( token, originalUrl ) ->
                    ( newModel
                    , Task.dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken token originalUrl)
                    )

                _ ->
                    ( newModel, Cmd.none )
    in
    case msg of
        GotOriginalUrl value ->
            case D.decodeValue (LocalStorageData.decoder (D.maybe D.string)) value of
                Ok localStorageData ->
                    if localStorageData.key == "wizard/originalUrl" then
                        ( { model | originalUrl = ActionResult.Success localStorageData.value }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AuthenticationCompleted result ->
            case result of
                Ok tokenResponse ->
                    case tokenResponse of
                        TokenResponse.Token token expiresAt ->
                            dispatchToken { model | token = ActionResult.Success (Token.create token expiresAt) }

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
                            Cmd.map wrapMsg (AuthApi.postConsents appState model.id hash model.sessionState SubmitConsentCompleted)
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
                            ( model, Task.dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken (Token.create token expiresAt) Nothing) )

                        _ ->
                            ( { model | submittingConsent = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }, Cmd.none )

                Err error ->
                    ( { model | submittingConsent = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )
