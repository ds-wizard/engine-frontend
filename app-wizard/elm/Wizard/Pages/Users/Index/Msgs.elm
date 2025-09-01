module Wizard.Pages.Users.Index.Msgs exposing (Msg(..))

import Result exposing (Result)
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Api.Models.User exposing (User)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg User)
