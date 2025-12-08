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
import Wizard.Api.Models.ProjectSettings exposing (ProjectSettings)
import Wizard.Pages.Projects.Common.ProjectMigrationCreateForm as ProjectMigrationCreateForm exposing (ProjectMigrationCreateForm)


type alias Model =
    { projectUuid : Uuid
    , project : ActionResult ProjectSettings
    , currentPackage : ActionResult KnowledgeModelPackageDetail
    , selectedPackage : Maybe KnowledgeModelPackageSuggestion
    , selectedPackageDetail : ActionResult KnowledgeModelPackageDetail
    , form : Form FormError ProjectMigrationCreateForm
    , knowledgeModelPackageTypeHintInputModel : TypeHintInput.Model KnowledgeModelPackageSuggestion
    , selectedTags : List String
    , useAllQuestions : Bool
    , savingMigration : ActionResult String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , lastFetchedPreview : Maybe String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { projectUuid = uuid
    , project = Loading
    , currentPackage = Loading
    , selectedPackage = Nothing
    , selectedPackageDetail = Loading
    , form = ProjectMigrationCreateForm.initEmpty
    , knowledgeModelPackageTypeHintInputModel = TypeHintInput.init "knowledgeModelPackage"
    , selectedTags = []
    , useAllQuestions = True
    , savingMigration = Unset
    , knowledgeModelPreview = Unset
    , lastFetchedPreview = Nothing
    }
