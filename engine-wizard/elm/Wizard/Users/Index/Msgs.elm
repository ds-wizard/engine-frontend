module Wizard.Users.Index.Msgs exposing (Msg(..))

import Result exposing (Result)
import Shared.Data.User exposing (User)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg User)
