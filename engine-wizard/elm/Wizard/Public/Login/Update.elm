module Wizard.Public.Login.Update exposing (update)

import ActionResult exposing (ActionResult(..))
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Locale exposing (lg)
import Wizard.Auth.Msgs
import Wizard.Common.Api.Tokens as TokensApi
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.JwtToken as JwtToken
import Wizard.Msgs
import Wizard.Public.Login.Models exposing (Model)
import Wizard.Public.Login.Msgs exposing (Msg(..))
import Wizard.Utils exposing (dispatch)


update : Msg -> (Msg -> Wizard.Msgs.Msg) -> AppState -> Model -> ( Model, Cmd Wizard.Msgs.Msg )
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


loginCompleted : AppState -> Model -> Result ApiError String -> ( Model, Cmd Wizard.Msgs.Msg )
loginCompleted appState model result =
    case result of
        Ok token ->
            case JwtToken.parse token of
                Just jwt ->
                    ( model, dispatch (Wizard.Msgs.AuthMsg <| Wizard.Auth.Msgs.Token token jwt model.originalUrl) )

                Nothing ->
                    ( { model | loggingIn = Error <| lg "apiError.tokens.fetchTokenError" appState }, Cmd.none )

        Err error ->
            ( { model | loggingIn = ApiError.toActionResult (lg "apiError.tokens.fetchTokenError" appState) error }, Cmd.none )
