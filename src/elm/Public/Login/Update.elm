module Public.Login.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Auth.Models exposing (parseJwt)
import Auth.Msgs
import Common.Api.Tokens as TokensApi
import Common.ApiError exposing (ApiError, getServerError)
import Common.AppState exposing (AppState)
import Common.Locale exposing (l, lg)
import Msgs
import Public.Login.Models exposing (Model)
import Public.Login.Msgs exposing (Msg(..))
import Utils exposing (dispatch)


update : Msg -> (Msg -> Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        DoLogin ->
            ( { model | loggingIn = Loading }
            , Cmd.map wrapMsg <| TokensApi.fetchToken model appState LoginCompleted
            )

        LoginCompleted result ->
            loginCompleted appState model result

        GetProfileInfoFailed error ->
            ( { model | loggingIn = error }, Cmd.none )


loginCompleted : AppState -> Model -> Result ApiError String -> ( Model, Cmd Msgs.Msg )
loginCompleted appState model result =
    case result of
        Ok token ->
            case parseJwt token of
                Just jwt ->
                    ( model, dispatch (Msgs.AuthMsg <| Auth.Msgs.Token token jwt) )

                Nothing ->
                    ( { model | loggingIn = Error <| lg "apiError.tokens.fetchTokenError" appState }, Cmd.none )

        Err error ->
            ( { model | loggingIn = getServerError error <| lg "apiError.tokens.fetchTokenError" appState }, Cmd.none )
