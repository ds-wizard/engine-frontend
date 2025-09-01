module Wizard.Pages.KMEditor.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Utils.Form.FormError exposing (FormError)
import Wizard.Api.Models.Branch exposing (Branch)
import Wizard.Components.Listing.Models as Listing
import Wizard.Pages.KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm exposing (BranchUpgradeForm)
import Wizard.Pages.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.Pages.KMEditor.Common.UpgradeModal as UpgradeModal


type alias Model =
    { branches : Listing.Model Branch
    , branchToBeDeleted : Maybe Branch
    , deletingKnowledgeModel : ActionResult String
    , creatingMigration : ActionResult String
    , branchToBeUpgraded : Maybe Branch
    , branchUpgradeForm : Form FormError BranchUpgradeForm
    , deletingMigration : ActionResult String
    , deleteModal : DeleteModal.Model
    , upgradeModal : UpgradeModal.Model
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { branches = Listing.initialModel paginationQueryString
    , branchToBeDeleted = Nothing
    , deletingKnowledgeModel = Unset
    , creatingMigration = Unset
    , branchToBeUpgraded = Nothing
    , branchUpgradeForm = BranchUpgradeForm.init
    , deletingMigration = Unset
    , deleteModal = DeleteModal.initialModel
    , upgradeModal = UpgradeModal.initialModel
    }
