module Auth.Update exposing (update)

import Auth.Models as AuthModel exposing (initialSession, parseJwt, setToken, setUser)
import Auth.Msgs as AuthMsgs
import Auth.Requests exposing (..)
import Common.Models exposing (getServerErrorJwt)
import Jwt
import Models exposing (Model)
import Msgs exposing (Msg)
import Ports
import Public.Login.Msgs
import Public.Msgs
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate, homeRoute)
import Users.Common.Models exposing (User)
import Utils exposing (dispatch)


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.Token token jwt ->
            let
                newModel =
                    { model | session = setToken model.session token, jwt = Just jwt }
            in
            ( newModel, getCurrentUserCmd newModel )

        AuthMsgs.GetCurrentUserCompleted result ->
            getCurrentUserCompleted model result

        AuthMsgs.Logout ->
            logout model


getCurrentUserCmd : Model -> Cmd Msg
getCurrentUserCmd model =
    getCurrentUser model.session
        |> toCmd AuthMsgs.GetCurrentUserCompleted Msgs.AuthMsg


getCurrentUserCompleted : Model -> Result Jwt.JwtError User -> ( Model, Cmd Msg )
getCurrentUserCompleted model result =
    case result of
        Ok user ->
            let
                newModel =
                    { model | session = setUser model.session user }
            in
            ( newModel
            , Cmd.batch
                [ Ports.storeSession <| Just newModel.session
                , cmdNavigate Welcome
                ]
            )

        Err error ->
            let
                msg =
                    getServerErrorJwt error "Loading profile info failed"
                        |> Public.Login.Msgs.GetProfileInfoFailed
                        |> Public.Msgs.LoginMsg
                        |> Msgs.PublicMsg
            in
            ( model, dispatch msg )


logout : Model -> ( Model, Cmd Msg )
logout model =
    let
        cmd =
            Cmd.batch [ Ports.clearSession (), cmdNavigate homeRoute ]
    in
    ( { model | session = initialSession }, cmd )
