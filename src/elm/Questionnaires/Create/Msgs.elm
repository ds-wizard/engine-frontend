module Questionnaires.Create.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Models exposing (PackageDetail)
import Questionnaires.Common.Models exposing (Questionnaire)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List PackageDetail))
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
