module Wizard.KnowledgeModels.Preview.Msgs exposing (Msg(..))

import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Questionnaire as Questionnaire


type Msg
    = FetchPreviewComplete (Result ApiError KnowledgeModel)
    | GetPackageComplete (Result ApiError PackageDetail)
    | GetLevelsComplete (Result ApiError (List Level))
    | GetMetricsComplete (Result ApiError (List Metric))
    | QuestionnaireMsg Questionnaire.Msg
