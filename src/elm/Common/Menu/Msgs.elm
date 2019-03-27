module Common.Menu.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Common.ApiError exposing (ApiError)
import Common.Menu.Models exposing (BuildInfo)


type Msg
    = SetReportIssueOpen Bool
    | SetAboutOpen Bool
    | GetBuildInfoCompleted (Result ApiError BuildInfo)
    | HelpMenuDropdownMsg Dropdown.State
    | ProfileMenuDropdownMsg Dropdown.State
