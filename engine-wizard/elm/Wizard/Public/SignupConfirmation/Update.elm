module Wizard.Public.SignupConfirmation.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Shared.Api.Users as UsersApi
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.SignupConfirmation.Models exposing (Model)
import Wizard.Public.SignupConfirmation.Msgs exposing (Msg(..))


fetchData : String -> String -> AppState -> Cmd Msg
fetchData uuid hash appState =
    UsersApi.putUserActivation uuid hash appState SendConfirmationCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg appState model =
    case msg of
        SendConfirmationCompleted result ->
            ( handleSendConfirmationCompleted appState result model, Cmd.none )


handleSendConfirmationCompleted : AppState -> Result ApiError () -> Model -> Model
handleSendConfirmationCompleted appState result model =
    case result of
        Ok _ ->
            { model | confirmation = Success "" }

        Err error ->
            { model | confirmation = ApiError.toActionResult appState (gettext "Sign up process failed." appState.locale) error }
