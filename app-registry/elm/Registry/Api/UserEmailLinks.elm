module Registry.Api.UserEmailLinks exposing (postForgottenTokenUserEmailLink)

import Common.Api.Request as Requests exposing (ToMsg)
import Registry.Data.AppState as AppState exposing (AppState)
import Registry.Data.Forms.ForgottenTokenForm as ForgottenTokenForm exposing (ForgottenTokenForm)


postForgottenTokenUserEmailLink : AppState -> ForgottenTokenForm -> ToMsg () msg -> Cmd msg
postForgottenTokenUserEmailLink appState forgottenTokenForm toMsg =
    let
        body =
            ForgottenTokenForm.encode forgottenTokenForm
    in
    Requests.postWhatever (AppState.toServerInfo appState) "/user-email-links" body toMsg
