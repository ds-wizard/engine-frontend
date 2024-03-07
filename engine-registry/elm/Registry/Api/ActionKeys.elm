module Registry.Api.ActionKeys exposing (postForgottenTokenActionKey)

import Registry.Api.Requests as Requests
import Registry.Data.AppState exposing (AppState)
import Registry.Data.Forms.ForgottenTokenForm as ForgottenTokenForm exposing (ForgottenTokenForm)
import Shared.Api exposing (ToMsg)


postForgottenTokenActionKey : AppState -> ForgottenTokenForm -> ToMsg () msg -> Cmd msg
postForgottenTokenActionKey appState forgottenTokenForm toMsg =
    let
        body =
            ForgottenTokenForm.encode forgottenTokenForm
    in
    Requests.postWhatever appState "/action-keys" body toMsg
