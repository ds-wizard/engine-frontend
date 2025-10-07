module Wizard.Pages.KnowledgeModels.Preview.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.Questionnaire exposing (Questionnaire)
import Wizard.Components.Questionnaire as Questionnaire


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)
    | GetPackageComplete (Result ApiError PackageDetail)
    | QuestionnaireMsg Questionnaire.Msg
    | CreateProjectMsg
    | PostQuestionnaireCompleted (Result ApiError Questionnaire)
    | PutQuestionnaireContentComplete Uuid (Result ApiError ())
