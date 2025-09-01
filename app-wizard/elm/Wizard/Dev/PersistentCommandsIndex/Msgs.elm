module Wizard.Dev.PersistentCommandsIndex.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.PersistentCommand exposing (PersistentCommand)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ListingMsg (Listing.Msg PersistentCommand)
    | RetryFailed
    | RetryFailedComplete (Result ApiError ())
    | RerunCommand PersistentCommand
    | SetIgnored PersistentCommand
    | UpdateComplete (Result ApiError ())
