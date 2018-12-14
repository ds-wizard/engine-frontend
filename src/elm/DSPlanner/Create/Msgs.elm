module DSPlanner.Create.Msgs exposing (Msg(..))

import DSPlanner.Common.Models exposing (Questionnaire)
import Form
import Jwt
import KMPackages.Common.Models exposing (PackageDetail)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | PostQuestionnaireCompleted (Result Jwt.JwtError Questionnaire)
