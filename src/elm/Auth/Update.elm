module Auth.Update exposing (..)

import Auth.Models as AuthModel exposing (User, initialSession, setToken, setUser)
import Auth.Msgs as AuthMsgs
import Auth.Requests exposing (..)
import Http
import Jwt
import Models exposing (Model)
import Msgs exposing (Msg)
import Ports
import Routing exposing (Route(..), cmdNavigate)


authUserCmd : Model -> Cmd Msg
authUserCmd model =
    Http.send AuthMsgs.GetTokenCompleted (authUser model.authModel) |> Cmd.map Msgs.AuthMsg


profileCmd : Model -> Cmd Msg
profileCmd model =
    Jwt.send AuthMsgs.GetProfileCompleted (getCurrentUser model.session) |> Cmd.map Msgs.AuthMsg


getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok token ->
            ( { model | session = setToken model.session token, authModel = AuthModel.initialModel }
            , profileCmd model
            )

        Err error ->
            ( { model | authModel = AuthModel.updateLoading (AuthModel.updateError model.authModel "Login failed") False }, Cmd.none )


getProfileCompleted : Model -> Result Jwt.JwtError User -> ( Model, Cmd msg )
getProfileCompleted model result =
    case result of
        Ok user ->
            let
                newModel =
                    { model | session = setUser model.session user }
            in
            ( newModel
            , Cmd.batch
                [ Ports.storeSession <| Just newModel.session
                , cmdNavigate Index
                ]
            )

        Err error ->
            ( { model | authModel = AuthModel.updateLoading (AuthModel.updateError model.authModel "Loading profile info failed") False }, Cmd.none )


update : AuthMsgs.Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AuthMsgs.Email email ->
            ( { model | authModel = AuthModel.updateEmail model.authModel email }, Cmd.none )

        AuthMsgs.Password password ->
            ( { model | authModel = AuthModel.updatePassword model.authModel password }, Cmd.none )

        AuthMsgs.Login ->
            ( { model | authModel = AuthModel.updateLoading model.authModel True }, authUserCmd model )

        AuthMsgs.GetTokenCompleted result ->
            getTokenCompleted model result

        AuthMsgs.GetProfileCompleted result ->
            getProfileCompleted model result

        AuthMsgs.Logout ->
            ( { model | session = initialSession }
            , Cmd.batch
                [ Ports.clearSession ()
                , cmdNavigate Login
                ]
            )
