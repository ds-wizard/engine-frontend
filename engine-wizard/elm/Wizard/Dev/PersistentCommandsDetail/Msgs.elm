module Wizard.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))

import Bootstrap.Dropdown as Dropdown
import Shared.Data.PersistentCommandDetail exposing (PersistentCommandDetail)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GerPersistentCommandComplete (Result ApiError PersistentCommandDetail)
    | DropdownMsg Dropdown.State
    | RerunCommand
    | RerunCommandComplete (Result ApiError ())
    | SetIgnored
    | SetIgnoredComplete (Result ApiError ())
