module Wizard.Projects.CreateMigration.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.PackageSuggestion exposing (PackageSuggestion)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Common.Components.TypeHintInput as TypeHintInput
import Wizard.Projects.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm exposing (QuestionnaireMigrationCreateForm)


type alias Model =
    { questionnaireUuid : Uuid
    , questionnaire : ActionResult QuestionnaireDetail
    , selectedPackage : Maybe PackageSuggestion
    , form : Form FormError QuestionnaireMigrationCreateForm
    , packageTypeHintInputModel : TypeHintInput.Model PackageSuggestion
    , selectedTags : List String
    , savingMigration : ActionResult String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , lastFetchedPreview : Maybe String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { questionnaireUuid = uuid
    , questionnaire = Loading
    , selectedPackage = Nothing
    , form = QuestionnaireMigrationCreateForm.initEmpty
    , packageTypeHintInputModel = TypeHintInput.init "packageId"
    , selectedTags = []
    , savingMigration = Unset
    , knowledgeModelPreview = Unset
    , lastFetchedPreview = Nothing
    }
