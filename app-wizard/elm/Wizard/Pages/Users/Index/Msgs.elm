module Wizard.Pages.Users.Index.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.Pagination exposing (Pagination)
import Common.Api.Models.Role exposing (Role)
import Wizard.Api.Models.User exposing (User)
import Wizard.Components.Listing.Msgs as Listing


type Msg
    = GetRolesCompleted (Result ApiError (Pagination Role))
    | ShowHideDeleteUser (Maybe User)
    | DeleteUser
    | DeleteUserCompleted (Result ApiError ())
    | ListingMsg (Listing.Msg User)
