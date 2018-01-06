module Auth.Update exposing (update)

{-|

@docs update

-}

import Auth.Models as AuthModel exposing (initialSession, parseJwt, setToken, setUser)
import Auth.Msgs as AuthMsgs
import Auth.Requests exposing (..)
import Http
import Jwt
import Models exposing (Model)
import Msgs exposing (Msg)
import Ports
import Requests exposing (toCmd)
import Routing exposing (Route(..), cmdNavigate)
import UserManagement.Models exposing (User)


authUserCmd : Model -> Cmd Msg
authUserCmd model =
    authUser model.authModel
        |> Http.send AuthMsgs.AuthUserCompleted
        |> Cmd.map Msgs.AuthMsg


getCurrentUserCmd : Model -> Cmd Msg
getCurrentUserCmd model =
    getCurrentUser model.session
        |> toCmd AuthMsgs.GetCurrentUserCompleted Msgs.AuthMsg


authUserCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
authUserCompleted model result =
    case result of
        Ok token ->
            case parseJwt token of
                Just jwt ->
                    let
                        newModel =
                            { model
                                | session = setToken model.session token
                                , jwt = Just jwt
                            }
                    in
                    ( newModel, getCurrentUserCmd newModel )

                Nothing ->
                    ( { model | authModel = AuthModel.updateLoading (AuthModel.updateError model.authModel "Invalid response from the server") False }, Cmd.none )

        Err error ->
            ( { model | authModel = AuthModel.updateLoading (AuthModel.updateError model.authModel "Login failed") False }, Cmd.none )


getCurrentUserCompleted : Model -> Result Jwt.JwtError User -> ( Model, Cmd msg )
getCurrentUserCompleted model result =
    case result of
        Ok user ->
            let
                newModel =
                    { model | session = setUser model.session user, authModel = AuthModel.initialModel }
            in
            ( newModel
            , Cmd.batch
                [ Ports.storeSession <| Just newModel.session
                , cmdNavigate Index
                ]
            )

        Err error ->
            ( { model | authModel = AuthModel.updateLoading (AuthModel.updateError model.authModel "Loading profile info failed") False }, Cmd.none )


logout : Model -> ( Model, Cmd Msg )
logout model =
    let
        cmd =
            Cmd.batch [ Ports.clearSession (), cmdNavigate Login ]
    in
    ( { model | session = initialSession }, cmd )


{-| -}
update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.Email email ->
            ( { model | authModel = AuthModel.updateEmail model.authModel email }, Cmd.none )

        AuthMsgs.Password password ->
            ( { model | authModel = AuthModel.updatePassword model.authModel password }, Cmd.none )

        AuthMsgs.Login ->
            ( { model | authModel = AuthModel.updateLoading model.authModel True }, authUserCmd model )

        AuthMsgs.AuthUserCompleted result ->
            authUserCompleted model result

        AuthMsgs.GetCurrentUserCompleted result ->
            getCurrentUserCompleted model result

        AuthMsgs.Logout ->
            logout model
