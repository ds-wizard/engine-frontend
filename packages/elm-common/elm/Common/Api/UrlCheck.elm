module Common.Api.UrlCheck exposing (postUrlCheck)

import Common.Api.Context exposing (Context)
import Common.Api.Models.UrlCheckRequest as UrlCheckRequest exposing (UrlCheckRequest)
import Common.Api.Models.UrlCheckResponse as UrlCheckResponse exposing (UrlCheckResponse)
import Common.Api.Request as Request exposing (ToMsg)
import String.Extra as String


postUrlCheck : Context ctx -> UrlCheckRequest -> ToMsg UrlCheckResponse msg -> Cmd msg
postUrlCheck appState body =
    let
        serverInfo =
            { apiUrl = Maybe.withDefault "" appState.urlCheckerUrl
            , token = String.toMaybe appState.session.token.token
            }
    in
    Request.post serverInfo "" UrlCheckResponse.decoder (UrlCheckRequest.encode body)
