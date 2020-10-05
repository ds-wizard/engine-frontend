module Wizard.Projects.CreateMigration.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.Package exposing (Package)
import Shared.Data.QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Form.FormError exposing (FormError)
import Uuid exposing (Uuid)
import Wizard.Projects.Common.QuestionnaireMigrationCreateForm as QuestionnaireMigrationCreateForm exposing (QuestionnaireMigrationCreateForm)


type alias Model =
    { questionnaireUuid : Uuid
    , packages : ActionResult (List Package)
    , questionnaire : ActionResult QuestionnaireDetail
    , selectedPackage : Maybe Package
    , form : Form FormError QuestionnaireMigrationCreateForm
    , selectedTags : List String
    , savingMigration : ActionResult String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , lastFetchedPreview : Maybe String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { questionnaireUuid = uuid
    , packages = Loading
    , questionnaire = Loading
    , selectedPackage = Nothing
    , form = QuestionnaireMigrationCreateForm.initEmpty
    , selectedTags = []
    , savingMigration = Unset
    , knowledgeModelPreview = Unset
    , lastFetchedPreview = Nothing
    }
