module Wizard.Public.Login.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Json.Encode as E
import Shared.Api.Tokens as TokensApi
import Shared.Data.Token exposing (Token)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Shared.Utils exposing (dispatch)
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Msgs
import Wizard.Public.Login.Models exposing (Model)
import Wizard.Public.Login.Msgs exposing (Msg(..))


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
update msg wrapMsg appState model =
    case msg of
        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        DoLogin ->
            let
                body =
                    E.object
                        [ ( "email", E.string model.email )
                        , ( "password", E.string model.password )
                        ]
            in
            ( { model | loggingIn = Loading }
            , Cmd.map wrapMsg <| TokensApi.fetchToken body appState LoginCompleted
            )

        LoginCompleted result ->
            loginCompleted appState model result

        GetProfileInfoFailed error ->
            ( { model | loggingIn = error }, Cmd.none )


loginCompleted : AppState -> Model -> Result ApiError Token -> ( Model, Cmd Wizard.Msgs.Msg )
loginCompleted appState model result =
    case result of
        Ok token ->
            ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.GotToken token model.originalUrl) )

        Err error ->
            ( { model | loggingIn = ApiError.toActionResult appState (lg "apiError.tokens.fetchTokenError" appState) error }, Cmd.none )
