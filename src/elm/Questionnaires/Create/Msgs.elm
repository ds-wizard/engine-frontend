module Questionnaires.Create.Msgs exposing (Msg(..))

import Common.ApiError exposing (ApiError)
import Form
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)
import Questionnaires.Common.Questionnaire exposing (Questionnaire)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List Package))
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
