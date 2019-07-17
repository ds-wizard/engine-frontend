module Questionnaires.Edit.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Questionnaires.Common.QuestionnaireDetail exposing (QuestionnaireDetail)
import Questionnaires.Common.QuestionnaireEditForm as QuestionnaireEditForm exposing (QuestionnaireEditForm)


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
