module Auth.Update exposing (..)

import Auth.Models exposing (..)
import Auth.Msgs as AuthMsgs
import Auth.Requests exposing (..)
import Http
import Msgs exposing (Msg)
import Ports
import Routing exposing (Route(..), cmdNavigate)


authUserCmd : Model -> Cmd Msg
authUserCmd model =
    Http.send AuthMsgs.GetTokenCompleted (authUser model) |> Cmd.map Msgs.AuthMsg


getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd msg )
getTokenCompleted model result =
    case result of
        Ok token ->
            ( { model | token = token, password = "", error = "" }
            , Cmd.batch
                [ Ports.storeSession <| Just token
                , cmdNavigate Index
                ]
            )

        Err error ->
            ( { model | error = "Login failed" }, Cmd.none )


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.Email email ->
            ( { model | email = email }, Cmd.none )

        AuthMsgs.Password password ->
            ( { model | password = password }, Cmd.none )

        AuthMsgs.Login ->
            ( model, authUserCmd model )

        AuthMsgs.GetTokenCompleted result ->
            getTokenCompleted model result
