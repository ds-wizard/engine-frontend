module Organization.Msgs exposing (..)

import Form
import Jwt
import Organization.Models exposing (Organization)


type Msg
    = GetCurrentOrganizationCompleted (Result Jwt.JwtError Organization)
    | PutCurrentOrganizationCompleted (Result Jwt.JwtError String)
    | FormMsg Form.Msg
