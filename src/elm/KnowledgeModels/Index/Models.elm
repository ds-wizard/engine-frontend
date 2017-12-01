module KnowledgeModels.Index.Models exposing (..)

import Common.Types exposing (ActionResult(..))
import Form exposing (Form)
import KnowledgeModels.Models exposing (KnowledgeModel, KnowledgeModelUpgradeForm, initKnowledgeModelUpgradeForm)
import PackageManagement.Models exposing (PackageDetail)


type alias Model =
    { knowledgeModels : ActionResult (List KnowledgeModel)
    , kmToBeDeleted : Maybe KnowledgeModel
    , deletingKnowledgeModel : ActionResult String
    , creatingMigration : ActionResult String
    , kmToBeUpgraded : Maybe KnowledgeModel
    , packages : ActionResult (List PackageDetail)
    , kmUpgradeForm : Form () KnowledgeModelUpgradeForm
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
