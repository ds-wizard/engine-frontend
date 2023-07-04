module Wizard.Projects.Create.CustomCreate.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Common.QuestionnaireCustomCreateForm as QuestionnaireCustomCreateForm exposing (QuestionnaireCustomCreateForm)


type alias Model =
    { savingQuestionnaire : ActionResult String
    , form : Form FormError QuestionnaireCustomCreateForm
    , packageTypeHintInputModel : TypeHintInput.Model PackageSuggestion
    , selectedPackage : Maybe String
    , selectedTags : List String
    , useAllQuestions : Bool
    , lastFetchedPreview : Maybe String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    }


initialModel : AppState -> Maybe String -> Model
initialModel appState selectedPackage =
    { savingQuestionnaire = Unset
    , form = QuestionnaireCustomCreateForm.init appState selectedPackage
    , packageTypeHintInputModel = TypeHintInput.init "packageId"
    , selectedPackage = selectedPackage
    , selectedTags = []
    , useAllQuestions = True
    , lastFetchedPreview = selectedPackage
    , knowledgeModelPreview = Unset
    }
