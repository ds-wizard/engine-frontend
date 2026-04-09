module Wizard.Pages.KnowledgeModels.Preview.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.Project exposing (Project)
import Wizard.Components.Questionnaire2 as Questionnaire2


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)
    | GetPackageComplete (Result ApiError KnowledgeModelPackageDetail)
    | QuestionnaireMsg Questionnaire2.Msg
    | CreateProjectMsg
    | PostQuestionnaireCompleted (Result ApiError Project)
    | PutQuestionnaireContentComplete Uuid (Result ApiError ())
