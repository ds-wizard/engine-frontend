module Wizard.Pages.Locales.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Wizard.Api.Models.LocaleDetail exposing (LocaleDetail)


type alias Model =
    { id : String
    , locale : ActionResult LocaleDetail
    , dropdownState : Dropdown.State
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    }


initialModel : String -> Model
initialModel id =
    { id = id
    , locale = Loading
    , dropdownState = Dropdown.initialState
    , deletingVersion = Unset
    , showDeleteDialog = False
    }
