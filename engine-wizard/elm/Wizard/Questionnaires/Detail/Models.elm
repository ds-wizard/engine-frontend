module Wizard.Questionnaires.Detail.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Wizard.Common.Questionnaire.Models
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)


type alias Model =
    { uuid : String
    , questionnaireModel : ActionResult Wizard.Common.Questionnaire.Models.Model
    , questionnaireDetail : ActionResult QuestionnaireDetail
    , levels : ActionResult (List Level)
    , metrics : ActionResult (List Metric)
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaireModel = Loading
    , questionnaireDetail = Loading
    , levels = Loading
    , metrics = Loading
    , savingQuestionnaire = Unset
    }
