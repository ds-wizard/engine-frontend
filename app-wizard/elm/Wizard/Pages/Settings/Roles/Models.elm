module Wizard.Pages.Settings.Roles.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Common.Api.Models.Role exposing (Role)


type alias Model =
    { roles : ActionResult (List Role)
    , roleToBeDeleted : Maybe Role
    , deletingRole : ActionResult String
    }


initialModel : Model
initialModel =
    { roles = ActionResult.Loading
    , roleToBeDeleted = Nothing
    , deletingRole = ActionResult.Unset
    }
