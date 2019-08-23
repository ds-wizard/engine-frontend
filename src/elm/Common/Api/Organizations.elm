module Common.Api.Organizations exposing
    ( getCurrentOrganization
    , putCurrentOrganization
    )

import Common.Api exposing (ToMsg, jwtGet, jwtPut)
import Common.AppState exposing (AppState)
import Json.Encode exposing (Value)
import Organization.Common.Organization as Organization exposing (Organization)


getCurrentOrganization : AppState -> ToMsg Organization msg -> Cmd msg
getCurrentOrganization =
    jwtGet "/organizations/current" Organization.decoder


putCurrentOrganization : Value -> AppState -> ToMsg () msg -> Cmd msg
putCurrentOrganization =
    jwtPut "/organizations/current"
