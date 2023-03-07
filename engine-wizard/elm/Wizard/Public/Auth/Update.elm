module Wizard.Public.Auth.Update exposing (fetchData, update)

import ActionResult
import Gettext exposing (gettext)
import Shared.Api.Auth as AuthApi
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


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        AuthenticationCompleted result ->
            case result of
                Ok tokenReponse ->
                    case TokenResponse.toToken tokenReponse of
                        Just token ->
                            ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken token Nothing) )

                        Nothing ->
                            ( { model | authenticating = ActionResult.Success (gettext "Login failed." appState.locale) }, Cmd.none )

                Err error ->
                    ( { model | authenticating = ApiError.toActionResult appState (gettext "Login failed." appState.locale) error }, Cmd.none )
