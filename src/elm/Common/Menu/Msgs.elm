module Common.Menu.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.Menu.Models exposing (BuildInfo)
import Http


type Msg
    = SetReportIssueOpen Bool
    | SetAboutOpen Bool
    | GetBuildInfoCompleted (Result Http.Error BuildInfo)
    | ProfileMenuDropdownMsg Dropdown.State
