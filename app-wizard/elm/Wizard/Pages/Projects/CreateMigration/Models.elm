module Wizard.Pages.Projects.CreateMigration.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Utils.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.PackageDetail exposing (PackageDetail)
import Wizard.Api.Models.PackageSuggestion exposing (PackageSuggestion)
import Wizard.Api.Models.QuestionnaireSettings exposing (QuestionnaireSettings)
import Wizard.Components.TypeHintInput as TypeHintInput
import Wizard.Pages.Projects.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm exposing (QuestionnaireMigrationCreateForm)


type alias Model =
    { questionnaireUuid : Uuid
    , questionnaire : ActionResult QuestionnaireSettings
    , currentPackage : ActionResult PackageDetail
    , selectedPackage : Maybe PackageSuggestion
    , selectedPackageDetail : ActionResult PackageDetail
    , form : Form FormError QuestionnaireMigrationCreateForm
    , packageTypeHintInputModel : TypeHintInput.Model PackageSuggestion
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
    , packageTypeHintInputModel = TypeHintInput.init "package"
    , selectedTags = []
    , useAllQuestions = True
    , savingMigration = Unset
    , knowledgeModelPreview = Unset
    , lastFetchedPreview = Nothing
    }
