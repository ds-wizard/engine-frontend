module Wizard.Common.Menu.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Menu.Models exposing (BuildInfo)


type Msg
    = SetReportIssueOpen Bool
    | SetAboutOpen Bool
    | GetBuildInfoCompleted (Result ApiError BuildInfo)
    | HelpMenuDropdownMsg Dropdown.State
    | SettingsMenuDropdownMsg Dropdown.State
    | ProfileMenuDropdownMsg Dropdown.State
