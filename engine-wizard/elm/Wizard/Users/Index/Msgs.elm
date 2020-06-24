module Wizard.Users.Index.Msgs exposing (Msg(..))

import Result exposing (Result)
import Shared.Data.User exposing (User)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Common.Components.Listing as Listing


type Msg
    = GetUsersCompleted (Result ApiError (List User))
    | ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
    | ListingMsg Listing.Msg
