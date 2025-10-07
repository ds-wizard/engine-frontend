module Wizard.Pages.Users.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Result exposing (Result)
import Wizard.Api.Models.User exposing (User)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg User)
