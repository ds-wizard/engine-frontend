module Wizard.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))

import Shared.Data.PersistentCommand exposing (PersistentCommand)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg PersistentCommand)
    | RetryFailed
    | RetryFailedComplete (Result ApiError ())
    | RerunCommand PersistentCommand
    | SetIgnored PersistentCommand
    | UpdateComplete (Result ApiError ())
