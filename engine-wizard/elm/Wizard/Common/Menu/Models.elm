module Wizard.Common.Menu.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Shared.Data.BuildInfo exposing (BuildInfo)


type alias Model =
    { reportIssueOpen : Bool
    , devMenuDropdownState : Dropdown.State
    , helpMenuDropdownState : Dropdown.State
    , profileMenuDropdownState : Dropdown.State
    , aboutOpen : Bool
    , apiBuildInfo : ActionResult BuildInfo
    }


initialModel : Model
initialModel =
    { reportIssueOpen = False
    , devMenuDropdownState = Dropdown.initialState
    , helpMenuDropdownState = Dropdown.initialState
    , profileMenuDropdownState = Dropdown.initialState
    , aboutOpen = False
    , apiBuildInfo = Unset
    }
