module Organization.Requests exposing (..)

{-|

@docs getCurrentOrganization, putCurrentOrganization

-}

import Auth.Models exposing (Session)
import Http
import Json.Encode exposing (Value)
import Organization.Models exposing (Organization, organizationDecoder)
import Requests


{-| -}
getCurrentOrganization : Session -> Http.Request Organization
getCurrentOrganization session =
    Requests.get session "/organizations/current" organizationDecoder


{-| -}
putCurrentOrganization : Session -> Value -> Http.Request String
putCurrentOrganization session organization =
    Requests.put organization session "/organizations/current"
