module Wizard.Dev.PersistentCommandsDetail.Msgs exposing (Msg(..))

import Shared.Data.PersistentCommandDetail exposing (PersistentCommandDetail)
import Shared.Error.ApiError exposing (ApiError)


type Msg
    = GerPersistentCommandComplete (Result ApiError PersistentCommandDetail)
    | RerunCommand
    | RerunCommandComplete (Result ApiError ())
