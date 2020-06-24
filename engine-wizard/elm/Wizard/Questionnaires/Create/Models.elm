module Wizard.Questionnaires.Create.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package exposing (Package)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Common.QuestionnaireCreateForm as QuestionnaireCreateForm exposing (QuestionnaireCreateForm)


type alias Model =
    { packages : ActionResult (List Package)
    , savingQuestionnaire : ActionResult String
    , form : Form FormError QuestionnaireCreateForm
    , selectedPackage : Maybe String
    , selectedTags : List String
    , lastFetchedPreview : Maybe String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    }


initialModel : AppState -> Maybe String -> Model
initialModel appState selectedPackage =
    { packages = Loading
    , savingQuestionnaire = Unset
    , form = QuestionnaireCreateForm.init appState selectedPackage
    , selectedPackage = selectedPackage
    , selectedTags = []
    , lastFetchedPreview = Nothing
    , knowledgeModelPreview = Unset
    }
