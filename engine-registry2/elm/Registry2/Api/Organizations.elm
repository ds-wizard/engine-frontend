module Registry2.Api.Organizations exposing (getOrganization)

import Registry2.Api.Models.Organization as Organization exposing (Organization)
import Registry2.Api.Requests as Requests
import Registry2.Data.AppState exposing (AppState)
import Shared.Api exposing (ToMsg)


getOrganization : AppState -> String -> ToMsg Organization msg -> Cmd msg
getOrganization appState organizationId =
    Requests.get appState ("/organizations/" ++ organizationId) Organization.decoder
