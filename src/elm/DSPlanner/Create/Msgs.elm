module DSPlanner.Create.Msgs exposing (Msg(..))

import DSPlanner.Common.Models exposing (Questionnaire)
import Form
import Jwt
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KMPackages.Common.Models exposing (PackageDetail)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | GetKnowledgeModelPreviewCompleted (Result Jwt.JwtError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | PostQuestionnaireCompleted (Result Jwt.JwtError Questionnaire)
