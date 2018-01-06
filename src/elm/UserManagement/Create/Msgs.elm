module UserManagement.Create.Msgs exposing (..)

{-|

@docs Msg

-}

import Form
import Jwt


{-| -}
type Msg
    = FormMsg Form.Msg
    | PostUserCompleted (Result Jwt.JwtError String)
