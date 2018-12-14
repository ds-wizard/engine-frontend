module KMEditor.Create.Msgs exposing (Msg(..))

import Form
import Jwt
import KMEditor.Common.Models exposing (KnowledgeModel)
import KMPackages.Common.Models exposing (PackageDetail)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | PostKnowledgeModelCompleted (Result Jwt.JwtError KnowledgeModel)
