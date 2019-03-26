module Common.Api.Organizations exposing
    ( getCurrentOrganization
    , putCurrentOrganization
    )

import Common.Api exposing (ToMsg, jwtGet, jwtPut)
import Common.AppState exposing (AppState)
import Json.Encode exposing (Value)
import Organization.Models exposing (Organization, organizationDecoder)


getCurrentOrganization : AppState -> ToMsg Organization msg -> Cmd msg
getCurrentOrganization =
    jwtGet "/organizations/current" organizationDecoder


putCurrentOrganization : Value -> AppState -> ToMsg () msg -> Cmd msg
putCurrentOrganization =
    jwtPut "/organizations/current"
