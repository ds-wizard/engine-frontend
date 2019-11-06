module Wizard.Public.SignupConfirmation.Update exposing (fetchData, handleSendConfirmationCompleted, update)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Wizard.Common.Api.Users as UsersApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Locale exposing (lg)
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
            { model | confirmation = ApiError.toActionResult (lg "apiError.users.activation.putError" appState) error }
