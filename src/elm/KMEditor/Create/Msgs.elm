module KMEditor.Create.Msgs exposing (..)

{-|

@docs Msg

-}

import Form
import Jwt
import KMPackages.Models exposing (PackageDetail)


{-| -}
type Msg
    = NoOp
    | FormMsg Form.Msg
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | PostKnowledgeModelCompleted (Result Jwt.JwtError String)
