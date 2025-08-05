module Wizard.DocumentTemplates.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Wizard.Api.Models.DocumentTemplateDetail exposing (DocumentTemplateDetail)


type alias Model =
    { template : ActionResult DocumentTemplateDetail
    , dropdownState : Dropdown.State
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    , showAllKms : Bool
    }


initialModel : Model
initialModel =
    { template = Loading
    , dropdownState = Dropdown.initialState
    , deletingVersion = Unset
    , showDeleteDialog = False
    , showAllKms = False
    }
