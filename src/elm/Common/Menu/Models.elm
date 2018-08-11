module Common.Menu.Models exposing (..)

import Bootstrap.Dropdown as Dropdown


type alias Model =
    { reportIssueOpen : Bool
    , profileMenuDropdownState : Dropdown.State
    }


initialModel : Model
initialModel =
    { reportIssueOpen = False
    , profileMenuDropdownState = Dropdown.initialState
    }
