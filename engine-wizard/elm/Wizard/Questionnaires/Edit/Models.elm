module Wizard.Questionnaires.Edit.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Questionnaires.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)


type alias Model =
    { uuid : Uuid
    , questionnaire : ActionResult QuestionnaireDetail
    , editForm : Form FormError QuestionnaireEditForm
    , savingQuestionnaire : ActionResult String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaire = Loading
    , editForm = QuestionnaireEditForm.initEmpty
    , savingQuestionnaire = Unset
    }
