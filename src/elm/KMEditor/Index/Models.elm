module KMEditor.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Form exposing (CustomFormError)
import Form exposing (Form)
import KMEditor.Common.Branch exposing (Branch)
import KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm exposing (BranchUpgradeForm)
import KnowledgeModels.Common.PackageDetail exposing (PackageDetail)


type alias Model =
    { branches : ActionResult (List Branch)
    , branchToBeDeleted : Maybe Branch
    , deletingKnowledgeModel : ActionResult String
    , creatingMigration : ActionResult String
    , branchToBeUpgraded : Maybe Branch
    , package : ActionResult PackageDetail
    , branchUpgradeForm : Form CustomFormError BranchUpgradeForm
    , deletingMigration : ActionResult String
    }


initialModel : Model
initialModel =
    { branches = Loading
    , branchToBeDeleted = Nothing
    , deletingKnowledgeModel = Unset
    , creatingMigration = Unset
    , branchToBeUpgraded = Nothing
    , package = Unset
    , branchUpgradeForm = BranchUpgradeForm.init
    , deletingMigration = Unset
    }
