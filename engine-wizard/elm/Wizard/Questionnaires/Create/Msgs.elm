module Wizard.Questionnaires.Create.Msgs exposing (Msg(..))

import Form
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package exposing (Package)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = FormMsg Form.Msg
    | GetPackagesCompleted (Result ApiError (List Package))
    | GetKnowledgeModelPreviewCompleted (Result ApiError KnowledgeModel)
    | AddTag String
    | RemoveTag String
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
