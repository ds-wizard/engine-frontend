module Wizard.KMEditor.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Form exposing (Form)
import Shared.Data.Branch exposing (Branch)
import Shared.Data.PackageDetail exposing (PackageDetail)
import Shared.Data.PaginationQueryString exposing (PaginationQueryString)
import Shared.Form.FormError exposing (FormError)
import Wizard.Common.Components.Listing.Models as Listing
import Wizard.KMEditor.Common.BranchUpgradeForm as BranchUpgradeForm exposing (BranchUpgradeForm)


type alias Model =
    { branches : Listing.Model Branch
    , branchToBeDeleted : Maybe Branch
    , deletingKnowledgeModel : ActionResult String
    , creatingMigration : ActionResult String
    , branchToBeUpgraded : Maybe Branch
    , package : ActionResult PackageDetail
    , branchUpgradeForm : Form FormError BranchUpgradeForm
    , deletingMigration : ActionResult String
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { branches = Listing.initialModel paginationQueryString
    , branchToBeDeleted = Nothing
    , deletingKnowledgeModel = Unset
    , creatingMigration = Unset
    , branchToBeUpgraded = Nothing
    , package = Unset
    , branchUpgradeForm = BranchUpgradeForm.init
    , deletingMigration = Unset
    }
