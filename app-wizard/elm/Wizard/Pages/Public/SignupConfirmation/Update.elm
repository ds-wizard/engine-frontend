module Wizard.Pages.Public.SignupConfirmation.Update exposing (fetchData, update)

import ActionResult exposing (ActionResult(..))
import Common.Data.ApiError as ApiError exposing (ApiError)
import Gettext exposing (gettext)
import Wizard.Api.Users as UsersApi
import Wizard.Data.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Pages.Public.SignupConfirmation.Models exposing (Model)
import Wizard.Pages.Public.SignupConfirmation.Msgs exposing (Msg(..))


fetchData : String -> String -> AppState -> Cmd Msg
fetchData uuid hash appState =
    UsersApi.putUserActivation appState uuid hash SendConfirmationCompleted


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
