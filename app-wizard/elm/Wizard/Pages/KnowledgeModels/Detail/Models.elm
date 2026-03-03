module Wizard.Pages.KnowledgeModels.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Pages.KnowledgeModels.Common.DeleteModal as DeleteModal


type alias Model =
    { knowledgeModelPackage : ActionResult KnowledgeModelPackageDetail
    , dropdownState : Dropdown.State
    , deleteModalModel : DeleteModal.Model
    , showAllVersions : Bool
    }


initialModel : Model
initialModel =
    { knowledgeModelPackage = Loading
    , dropdownState = Dropdown.initialState
    , deleteModalModel = DeleteModal.initialModel False
    , showAllVersions = False
    }
