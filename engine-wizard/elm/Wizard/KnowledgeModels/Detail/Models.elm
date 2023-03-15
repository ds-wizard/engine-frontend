module Wizard.KnowledgeModels.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Shared.Data.PackageDetail exposing (PackageDetail)


type alias Model =
    { package : ActionResult PackageDetail
    , dropdownState : Dropdown.State
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    }


initialModel : Model
initialModel =
    { package = Loading
    , dropdownState = Dropdown.initialState
    , deletingVersion = Unset
    , showDeleteDialog = False
    }
