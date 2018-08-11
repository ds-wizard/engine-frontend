module Common.Menu.Msgs exposing (..)

import Bootstrap.Dropdown as Dropdown


type Msg
    = SetReportIssueOpen Bool
    | ProfileMenuDropdownMsg Dropdown.State
