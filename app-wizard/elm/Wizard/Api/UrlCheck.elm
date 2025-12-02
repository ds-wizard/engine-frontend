module Wizard.Api.UrlCheck exposing (postUrlCheck)

import Common.Api.Request as Request exposing (ToMsg)
import String.Extra as String
import Wizard.Api.Models.UrlCheckRequest as UrlCheckRequest exposing (UrlCheckRequest)
import Wizard.Api.Models.UrlCheckResponse as UrlCheckResponse exposing (UrlCheckResponse)
import Wizard.Data.AppState exposing (AppState)


postUrlCheck : AppState -> UrlCheckRequest -> ToMsg UrlCheckResponse msg -> Cmd msg
postUrlCheck appState body =
    let
        serverInfo =
            { apiUrl = Maybe.withDefault "" appState.urlCheckerUrl
            , token = String.toMaybe appState.session.token.token
            }
    in
    Request.post serverInfo "" UrlCheckResponse.decoder (UrlCheckRequest.encode body)
