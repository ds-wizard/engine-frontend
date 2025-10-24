module Wizard.Pages.KnowledgeModels.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)


type alias Model =
    { knowledgeModelPackage : ActionResult KnowledgeModelPackageDetail
    , dropdownState : Dropdown.State
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    , showAllVersions : Bool
    }


initialModel : Model
initialModel =
    { knowledgeModelPackage = Loading
    , dropdownState = Dropdown.initialState
    , deletingVersion = Unset
    , showDeleteDialog = False
    , showAllVersions = False
    }
