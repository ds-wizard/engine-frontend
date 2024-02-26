module Registry2.Api.ActionKeys exposing (postForgottenTokenActionKey)

import Registry2.Api.Requests as Requests
import Registry2.Data.AppState exposing (AppState)
import Registry2.Data.Forms.ForgottenTokenForm as ForgottenTokenForm exposing (ForgottenTokenForm)
import Shared.Api exposing (ToMsg)


postForgottenTokenActionKey : AppState -> ForgottenTokenForm -> ToMsg () msg -> Cmd msg
postForgottenTokenActionKey appState forgottenTokenForm toMsg =
    let
        body =
            ForgottenTokenForm.encode forgottenTokenForm
    in
    Requests.postWhatever appState "/action-keys" body toMsg
