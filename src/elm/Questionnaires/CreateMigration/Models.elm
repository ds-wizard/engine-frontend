module Questionnaires.CreateMigration.Models exposing
    ( Model
    , encodeQuestionnaireMigrationCreateForm
    , initialModel
    , questionnaireMigrationCreateFormValidation
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Common.Questionnaire.Models exposing (QuestionnaireDetail)
import Form exposing (Form)
import Form.Validate as Validate exposing (Validation)
import Json.Encode as E
import KMEditor.Common.Models.Entities exposing (KnowledgeModel)
import KnowledgeModels.Common.Package exposing (Package)


type alias Model =
    { questionnaireUuid : String
    , packages : ActionResult (List Package)
    , questionnaire : ActionResult QuestionnaireDetail
    , selectedPackage : Maybe Package
    , form : Form CustomFormError QuestionnaireMigrationCreateForm
    , selectedTags : List String
    , savingMigration : ActionResult String
    , knowledgeModelPreview : ActionResult KnowledgeModel
    , lastFetchedPreview : Maybe String
    }


initialModel : String -> Model
initialModel uuid =
    { questionnaireUuid = uuid
    , packages = Loading
    , questionnaire = Loading
    , selectedPackage = Nothing
    , form = initQuestionnaireMigrationCreateForm
    , selectedTags = []
    , savingMigration = Unset
    , knowledgeModelPreview = Unset
    , lastFetchedPreview = Nothing
    }


type alias QuestionnaireMigrationCreateForm =
    { packageId : String
    }


initQuestionnaireMigrationCreateForm : Form CustomFormError QuestionnaireMigrationCreateForm
initQuestionnaireMigrationCreateForm =
    Form.initial [] questionnaireMigrationCreateFormValidation


questionnaireMigrationCreateFormValidation : Validation CustomFormError QuestionnaireMigrationCreateForm
questionnaireMigrationCreateFormValidation =
    Validate.map QuestionnaireMigrationCreateForm
        (Validate.field "packageId" Validate.string)


encodeQuestionnaireMigrationCreateForm : List String -> QuestionnaireMigrationCreateForm -> E.Value
encodeQuestionnaireMigrationCreateForm tagUuids form =
    E.object
        [ ( "targetPackageId", E.string form.packageId )
        , ( "targetTagUuids", E.list E.string tagUuids )
        ]
