module Wizard.Pages.Public.Auth.Update exposing (fetchData, update)

import ActionResult
import Common.Api.ApiError as ApiError
import Common.Api.Models.Token as Token
import Common.Ports.LocalStorage as LocalStorage
import Gettext exposing (gettext)
import Task.Extra as Task
import Wizard.Api.Auth as AuthApi
import Wizard.Api.Models.TokenResponse as TokenResponse
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Auth.Msgs
import Wizard.Pages.Public.Auth.Models exposing (Model)
import Wizard.Pages.Public.Auth.Msgs exposing (Msg(..))


fetchData : String -> Maybe String -> Maybe String -> Maybe String -> AppState -> Cmd Msg
fetchData id mbError mbCode mbSessionState appState =
    Cmd.batch
        [ AuthApi.getToken appState id mbError mbCode mbSessionState AuthenticationCompleted
        , LocalStorage.getAndRemoveItem "wizard/originalUrl"
        ]


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    let
        dispatchToken newModel =
            case ActionResult.combine newModel.token newModel.originalUrl of
                ActionResult.Success ( token, originalUrl ) ->
                    ( newModel
                    , Task.dispatch (Wizard.Msgs.AuthMsg <| Wizard.Pages.Auth.Msgs.GotToken token originalUrl)
                    )

                _ ->
                    ( newModel, Cmd.none )
    in
    case msg of
        GotOriginalUrl localStorageItemResult ->
            case localStorageItemResult of
                Ok localStorageData ->
                    if localStorageData.key == "wizard/originalUrl" then
                        dispatchToken { model | originalUrl = ActionResult.Success localStorageData.value }

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
                            ( model, Task.dispatch (Wizard.Msgs.AuthMsg <| Wizard.Pages.Auth.Msgs.GotToken (Token.create token expiresAt) Nothing) )

                        _ ->
                            ( { model | submittingConsent = ActionResult.Error (gettext "Unexpected response from the server." appState.locale) }, Cmd.none )

                Err error ->
                    ( { model | submittingConsent = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )
