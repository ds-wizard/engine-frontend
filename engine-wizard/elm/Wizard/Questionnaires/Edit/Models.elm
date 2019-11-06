module Wizard.Questionnaires.Edit.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Wizard.Questionnaires.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)


type alias Model =
    { uuid : String
    , questionnaire : ActionResult QuestionnaireDetail
    , editForm : Form CustomFormError QuestionnaireEditForm
    , savingQuestionnaire : ActionResult String
    }


initialModel : String -> Model
initialModel uuid =
    { uuid = uuid
    , questionnaire = Loading
    , editForm = QuestionnaireEditForm.initEmpty
    , savingQuestionnaire = Unset
    }
