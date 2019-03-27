module Public.SignupConfirmation.Update exposing (fetchData, handleSendConfirmationCompleted, update)

import ActionResult exposing (ActionResult(..))
import Common.Api.Users as UsersApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Msgs
import Public.SignupConfirmation.Models exposing (Model)
import Public.SignupConfirmation.Msgs exposing (Msg(..))


fetchData : (Msg -> Msgs.Msg) -> String -> String -> AppState -> Cmd Msgs.Msg
fetchData wrapMsg uuid hash appState =
    Cmd.map wrapMsg <|
        UsersApi.putUserActivation uuid hash appState SendConfirmationCompleted


update : Msg -> Model -> ( Model, Cmd Msgs.Msg )
update msg model =
    case msg of
        SendConfirmationCompleted result ->
            ( handleSendConfirmationCompleted result model, Cmd.none )


handleSendConfirmationCompleted : Result ApiError () -> Model -> Model
handleSendConfirmationCompleted result model =
    case result of
        Ok _ ->
            { model | confirmation = Success "" }

        Err error ->
            { model | confirmation = getServerError error "Email could not be confirmed" }
