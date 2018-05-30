module KMEditor.Create.Msgs exposing (..)

import Form
import Jwt
import KMPackages.Common.Models exposing (PackageDetail)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | PostKnowledgeModelCompleted (Result Jwt.JwtError String)
