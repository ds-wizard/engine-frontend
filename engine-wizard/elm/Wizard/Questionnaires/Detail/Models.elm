module Wizard.Questionnaires.Detail.Models exposing (Model, initialModel, isDirty)

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Shared.Data.KnowledgeModel.Level exposing (Level)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Uuid exposing (Uuid)
import Wizard.Common.Questionnaire.Models
import Wizard.Questionnaires.Common.DeleteQuestionnaireModal.Models as DeleteQuestionnaireModal


type alias Model =
    { uuid : Uuid
    , questionnaireModel : ActionResult Wizard.Common.Questionnaire.Models.Model
    , questionnaireDetail : ActionResult QuestionnaireDetail
    , levels : ActionResult (List Level)
    , metrics : ActionResult (List Metric)
    , savingQuestionnaire : ActionResult String
    , actionsDropdownState : Dropdown.State
    , deleteModalModel : DeleteQuestionnaireModal.Model
    , cloningQuestionnaire : ActionResult String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaireModel = Loading
    , questionnaireDetail = Loading
    , levels = Loading
    , metrics = Loading
    , savingQuestionnaire = Unset
    , actionsDropdownState = Dropdown.initialState
    , deleteModalModel = DeleteQuestionnaireModal.initialModel
    , cloningQuestionnaire = Unset
    }


isDirty : Model -> Bool
isDirty =
    .questionnaireModel >> ActionResult.map .dirty >> ActionResult.withDefault False
