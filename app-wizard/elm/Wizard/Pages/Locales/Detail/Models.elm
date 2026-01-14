module Wizard.Pages.Locales.Detail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Uuid exposing (Uuid)
import Wizard.Api.Models.LocaleDetail exposing (LocaleDetail)


type alias Model =
    { uuid : Uuid
    , locale : ActionResult LocaleDetail
    , dropdownState : Dropdown.State
    , deletingVersion : ActionResult String
    , showDeleteDialog : Bool
    }


initialModel : Uuid -> Model
initialModel localeUuid =
    { uuid = localeUuid
    , locale = Loading
    , dropdownState = Dropdown.initialState
    , deletingVersion = Unset
    , showDeleteDialog = False
    }
