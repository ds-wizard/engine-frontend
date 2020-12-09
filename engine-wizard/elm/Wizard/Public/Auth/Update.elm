module Wizard.Public.Auth.Update exposing (..)

import Shared.Api.Auth as AuthApi
import Shared.Error.ApiError as ApiError
import Shared.Locale exposing (lg)
import Shared.Utils exposing (dispatch)
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.Auth.Models exposing (Model)
import Wizard.Public.Auth.Msgs exposing (Msg(..))


fetchData : String -> Maybe String -> Maybe String -> AppState -> Cmd Msg
fetchData id error code appState =
    AuthApi.getToken id error code appState AuthenticationCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        AuthenticationCompleted result ->
            case result of
                Ok token ->
                    ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken token Nothing) )

                Err error ->
                    ( { model | authenticating = ApiError.toActionResult appState (lg "apiError.tokens.fetchTokenError" appState) error }, Cmd.none )
