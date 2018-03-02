module Public.Login.Update exposing (..)

import Auth.Models exposing (parseJwt)
import Auth.Msgs
import Http
import Msgs
import Public.Login.Models exposing (Model)
import Public.Login.Msgs exposing (Msg(..))
import Public.Login.Requests exposing (login)
import Utils exposing (dispatch)


update : Msg -> (Msg -> Msgs.Msg) -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg model =
    case msg of
        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        Login ->
            ( { model | loading = True }, loginCmd wrapMsg model )

        LoginCompleted result ->
            loginCompleted model result

        GetProfileInfoFailed ->
            ( { model | loading = False, error = "Loading profile info failed" }, Cmd.none )


loginCmd : (Msg -> Msgs.Msg) -> Model -> Cmd Msgs.Msg
loginCmd wrapMsg model =
    Http.send (wrapMsg << LoginCompleted) (login model)


loginCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msgs.Msg )
loginCompleted model result =
    case result of
        Ok token ->
            case parseJwt token of
                Just jwt ->
                    ( model, dispatch (Msgs.AuthMsg <| Auth.Msgs.Token token jwt) )

                Nothing ->
                    ( { model | loading = False, error = "Invalid response from the server" }, Cmd.none )

        Err error ->
            ( { model | loading = False, error = "Login failed" }, Cmd.none )
