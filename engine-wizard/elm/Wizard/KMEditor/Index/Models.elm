module Wizard.KMEditor.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Wizard.Common.Components.Listing as Listing
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.KMEditor.Common.Branch exposing (Branch)
import Wizard.KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm exposing (BranchUpgradeForm)
import Wizard.KnowledgeModels.Common.PackageDetail exposing (PackageDetail)


type alias Model =
    { branches : ActionResult (Listing.Model Branch)
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
