module Registry.Api.ActionKeys exposing (postForgottenTokenActionKey)

import Registry.Data.AppState as AppState exposing (AppState)
import Registry.Data.Forms.ForgottenTokenForm as ForgottenTokenForm exposing (ForgottenTokenForm)
import Shared.Api.Request as Requests exposing (ToMsg)


postForgottenTokenActionKey : AppState -> ForgottenTokenForm -> ToMsg () msg -> Cmd msg
postForgottenTokenActionKey appState forgottenTokenForm toMsg =
    let
        body =
            ForgottenTokenForm.encode forgottenTokenForm
    in
    Requests.postWhatever (AppState.toServerInfo appState) "/action-keys" body toMsg
