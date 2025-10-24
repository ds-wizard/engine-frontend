module Wizard.Pages.Projects.CreateMigration.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Components.TypeHintInput as TypeHintInput
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Api.Models.KnowledgeModelPackageSuggestion exposing (KnowledgeModelPackageSuggestion)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Pages.Projects.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm exposing (QuestionnaireMigrationCreateForm)


type alias Model =
    { questionnaireUuid : Uuid
    , questionnaire : ActionResult QuestionnaireSettings
    , currentPackage : ActionResult KnowledgeModelPackageDetail
    , selectedPackage : Maybe KnowledgeModelPackageSuggestion
    , selectedPackageDetail : ActionResult KnowledgeModelPackageDetail
    , form : Form FormError QuestionnaireMigrationCreateForm
    , knowledgeModelPackageTypeHintInputModel : TypeHintInput.Model KnowledgeModelPackageSuggestion
    , selectedTags : List String
    , useAllQuestions : Bool
    , savingMigration : ActionResult String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , lastFetchedPreview : Maybe String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { questionnaireUuid = uuid
    , questionnaire = Loading
    , currentPackage = Loading
    , selectedPackage = Nothing
    , selectedPackageDetail = Loading
    , form = QuestionnaireMigrationCreateForm.initEmpty
    , knowledgeModelPackageTypeHintInputModel = TypeHintInput.init "knowledgeModelPackage"
    , selectedTags = []
    , useAllQuestions = True
    , savingMigration = Unset
    , knowledgeModelPreview = Unset
    , lastFetchedPreview = Nothing
    }
