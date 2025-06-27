module Wizard.Common.Menu.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Browser.Dom as Dom
import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.BuildInfo exposing (BuildInfo)


type Msg
    = SetReportIssueOpen Bool
    | SetAboutOpen Bool
    | CopyAbout
    | ClearRecentlyCopied
    | GetBuildInfoCompleted (Result ApiError BuildInfo)
    | DevMenuDropdownMsg Dropdown.State
    | HelpMenuDropdownMsg Dropdown.State
    | ProfileMenuDropdownMsg Dropdown.State
    | GetElement String
    | GotElement String (Result Dom.Error Dom.Element)
    | HideElement String
