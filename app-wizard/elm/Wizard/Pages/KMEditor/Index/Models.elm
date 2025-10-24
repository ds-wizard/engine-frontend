module Wizard.Pages.KMEditor.Index.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Common.Data.PaginationQueryString exposing (PaginationQueryString)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Wizard.Api.Models.KnowledgeModelEditor exposing (KnowledgeModelEditor)
import Wizard.Components.Listing.Models as Listing
import Wizard.Pages.KMEditor.Common.DeleteModal as DeleteModal
import Wizard.Pages.KMEditor.Common.KnowledgeModelEditorUpgradeForm as KnowledgeModelEditorUpgradeForm exposing (KnowledgeModelEditorUpgradeForm)
import Wizard.Pages.KMEditor.Common.UpgradeModal as UpgradeModal


type alias Model =
    { kmEditors : Listing.Model KnowledgeModelEditor
    , kmEditorToBeDeleted : Maybe KnowledgeModelEditor
    , deletingKnowledgeModel : ActionResult String
    , creatingMigration : ActionResult String
    , kmEditorToBeUpgraded : Maybe KnowledgeModelEditor
    , kmEditorUpgradeForm : Form FormError KnowledgeModelEditorUpgradeForm
    , deletingMigration : ActionResult String
    , deleteModal : DeleteModal.Model
    , upgradeModal : UpgradeModal.Model
    }


initialModel : PaginationQueryString -> Model
initialModel paginationQueryString =
    { kmEditors = Listing.initialModel paginationQueryString
    , kmEditorToBeDeleted = Nothing
    , deletingKnowledgeModel = Unset
    , creatingMigration = Unset
    , kmEditorToBeUpgraded = Nothing
    , kmEditorUpgradeForm = KnowledgeModelEditorUpgradeForm.init
    , deletingMigration = Unset
    , deleteModal = DeleteModal.initialModel
    , upgradeModal = UpgradeModal.initialModel
    }
