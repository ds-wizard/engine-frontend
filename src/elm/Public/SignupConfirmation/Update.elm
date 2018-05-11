module Public.SignupConfirmation.Update exposing (..)

import Common.Models exposing (getServerError)
import Common.Types exposing (ActionResult(..))
import Http
import Msgs
import Public.SignupConfirmation.Models exposing (Model)
import Public.SignupConfirmation.Msgs exposing (Msg(..))
import Public.SignupConfirmation.Requests exposing (putUserActivation)


fetchData : (Msg -> Msgs.Msg) -> String -> String -> Cmd Msgs.Msg
fetchData wrapMsg userId hash =
    putUserActivation userId hash
        |> Http.send SendConfirmationCompleted
        |> Cmd.map wrapMsg


update : Msg -> Model -> ( Model, Cmd Msgs.Msg )
update msg model =
    case msg of
        SendConfirmationCompleted result ->
            ( handleSendConfirmationCompleted result model, Cmd.none )


handleSendConfirmationCompleted : Result Http.Error String -> Model -> Model
handleSendConfirmationCompleted result model =
    case result of
        Ok _ ->
            { model | confirmation = Success "" }

        Err error ->
            { model | confirmation = getServerError error "Email could not be confirmed" }
