module KMEditor.Index.Models exposing (KnowledgeModelUpgradeForm, Model, encodeKnowledgeModelUpgradeForm, initKnowledgeModelUpgradeForm, initialModel, knowledgeModelUpgradeFormValidation)

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Json.Encode as Encode exposing (..)
import KMEditor.Common.Models exposing (KnowledgeModel)
import KnowledgeModels.Common.Models exposing (PackageDetail)


type alias Model =
    { knowledgeModels : ActionResult (List KnowledgeModel)
    , kmToBeDeleted : Maybe KnowledgeModel
    , deletingKnowledgeModel : ActionResult String
    , creatingMigration : ActionResult String
    , kmToBeUpgraded : Maybe KnowledgeModel
    , packages : ActionResult (List PackageDetail)
    , kmUpgradeForm : Form CustomFormError KnowledgeModelUpgradeForm
    , deletingMigration : ActionResult String
    }


initialModel : Model
initialModel =
    { knowledgeModels = Loading
    , kmToBeDeleted = Nothing
    , deletingKnowledgeModel = Unset
    , creatingMigration = Unset
    , kmToBeUpgraded = Nothing
    , packages = Unset
    , kmUpgradeForm = initKnowledgeModelUpgradeForm
    , deletingMigration = Unset
    }


type alias KnowledgeModelUpgradeForm =
    { targetPackageId : String
    }


initKnowledgeModelUpgradeForm : Form CustomFormError KnowledgeModelUpgradeForm
initKnowledgeModelUpgradeForm =
    Form.initial [] knowledgeModelUpgradeFormValidation


knowledgeModelUpgradeFormValidation : Validation CustomFormError KnowledgeModelUpgradeForm
knowledgeModelUpgradeFormValidation =
    Validate.map KnowledgeModelUpgradeForm
        (Validate.field "targetPackageId" Validate.string)


encodeKnowledgeModelUpgradeForm : KnowledgeModelUpgradeForm -> Encode.Value
encodeKnowledgeModelUpgradeForm form =
    Encode.object
        [ ( "targetPackageId", Encode.string form.targetPackageId ) ]
