module Wizard.Projects.Create.TemplateCreate.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Maybe.Extra as Maybe
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Common.QuestionnaireFromTemplateCreateForm as QuestionnaireFromTemplateCreateForm exposing (QuestionnaireFromTemplateCreateForm)


type alias Model =
    { savingQuestionnaire : ActionResult String
    , form : Form FormError QuestionnaireFromTemplateCreateForm
    , questionnaireTypeHintInputModel : TypeHintInput.Model Questionnaire
    , selectedQuestionnaire : Maybe String
    , templateQuestionnaire : ActionResult QuestionnaireDetail
    }


initialModel : Maybe String -> Model
initialModel selectedQuestionnaire =
    { savingQuestionnaire = Unset
    , form = QuestionnaireFromTemplateCreateForm.init selectedQuestionnaire
    , questionnaireTypeHintInputModel = TypeHintInput.init "uuid"
    , selectedQuestionnaire = selectedQuestionnaire
    , templateQuestionnaire =
        if Maybe.isJust selectedQuestionnaire then
            Loading

        else
            Unset
    }
