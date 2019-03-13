module Questionnaires.Create.Msgs exposing (Msg(..))

import Form
import Jwt
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Models exposing (PackageDetail)
import Questionnaires.Common.Models exposing (Questionnaire)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result Jwt.JwtError (List PackageDetail))
    | GetKnowledgeModelPreviewCompleted (Result Jwt.JwtError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | PostQuestionnaireCompleted (Result Jwt.JwtError Questionnaire)
