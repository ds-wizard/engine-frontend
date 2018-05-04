module Questionnaires.Create.Msgs exposing (..)

import Form
import Jwt
import PackageManagement.Models exposing (PackageDetail)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | PostQuestionnaireCompleted (Result Jwt.JwtError String)
