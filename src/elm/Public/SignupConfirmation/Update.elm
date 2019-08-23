module Public.SignupConfirmation.Update exposing (fetchData, handleSendConfirmationCompleted, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (lg)
import Msgs
import Public.SignupConfirmation.Models exposing (Model)
import Public.SignupConfirmation.Msgs exposing (Msg(..))


fetchData : String -> String -> AppState -> Cmd Msg
fetchData uuid hash appState =
    UsersApi.putUserActivation uuid hash appState SendConfirmationCompleted


update : Msg -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
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
            { model | confirmation = getServerError error <| lg "apiError.users.activation.putError" appState }
