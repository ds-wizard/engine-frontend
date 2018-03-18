module Public.SignupConfirmation.Update exposing (..)

import Common.Form exposing (getErrorMessage)
import Common.Types exposing (ActionResult(..))
import Http
import Msgs
import Public.SignupConfirmation.Models exposing (Model)
import Public.SignupConfirmation.Msgs exposing (Msg(..))
import Public.SignupConfirmation.Requests exposing (putUserActivation)


fetchData : (Msg -> Msgs.Msg) -> String -> String -> Cmd Msgs.Msg
fetchData wrapMsg userId hash =
    putUserActivationCmd userId hash |> Cmd.map wrapMsg


update : Msg -> Model -> ( Model, Cmd Msgs.Msg )
update msg model =
    case msg of
        SendConfirmationCompleted result ->
            handleSendConfirmationCompleted result model


putUserActivationCmd : String -> String -> Cmd Msg
putUserActivationCmd userId hash =
    putUserActivation userId hash |> Http.send SendConfirmationCompleted


handleSendConfirmationCompleted : Result Http.Error String -> Model -> ( Model, Cmd Msgs.Msg )
handleSendConfirmationCompleted result model =
    case result of
        Ok _ ->
            ( { model | confirmation = Success "" }, Cmd.none )

        Err error ->
            let
                errorMessage =
                    getErrorMessage error "Email could not be confirmed"
            in
            ( { model | confirmation = Error errorMessage }, Cmd.none )
