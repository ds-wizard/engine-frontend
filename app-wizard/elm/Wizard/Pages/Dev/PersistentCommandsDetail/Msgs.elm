module Wizard.Pages.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.PersistentCommandDetail exposing (PersistentCommandDetail)


type Msg
    = GerPersistentCommandComplete (Result ApiError PersistentCommandDetail)
    | DropdownMsg Dropdown.State
    | RerunCommand
    | RerunCommandComplete (Result ApiError ())
    | SetIgnored
    | SetIgnoredComplete (Result ApiError ())
