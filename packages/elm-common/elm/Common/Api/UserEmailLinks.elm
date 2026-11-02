module Common.Api.UserEmailLinks exposing (postUserEmailLink)

import Common.Api.Context as Context exposing (Context)
import Common.Api.Request as Request exposing (ToMsg)
import Json.Encode as E


postUserEmailLink : Context ctx -> E.Value -> ToMsg () msg -> Cmd msg
postUserEmailLink appState =
    Request.postWhatever (Context.toServerInfo appState) "/user-email-links"
