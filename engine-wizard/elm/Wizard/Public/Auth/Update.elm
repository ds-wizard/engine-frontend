module Wizard.Public.Auth.Update exposing (..)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError
import Shared.Locale exposing (lg)
import Wizard.Auth.Msgs
import Wizard.Common.Api.Auth as AuthApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken as JwtToken
import Wizard.Msgs
import Wizard.Public.Auth.Models exposing (Model)
import Wizard.Public.Auth.Msgs exposing (Msg(..))
import Wizard.Utils exposing (dispatch)


fetchData : String -> Maybe String -> Maybe String -> AppState -> Cmd Msg
fetchData id error code appState =
    AuthApi.getToken id error code appState AuthenticationCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        AuthenticationCompleted result ->
            case result of
                Ok token ->
                    case JwtToken.parse token of
                        Just jwt ->
                            ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.Token token jwt Nothing) )

                        Nothing ->
                            ( { model | authenticating = Error "It failed" }, Cmd.none )

                Err error ->
                    ( { model | authenticating = ApiError.toActionResult (lg "apiError.tokens.fetchTokenError" appState) error }, Cmd.none )
