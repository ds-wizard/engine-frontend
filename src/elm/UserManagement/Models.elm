module UserManagement.Models exposing (..)

import UserManagement.Create.Models
import UserManagement.Edit.Models
import UserManagement.Index.Models
import UserManagement.Routing exposing (Route(..))


type alias Model =
    { createModel : UserManagement.Create.Models.Model
    , editModel : UserManagement.Edit.Models.Model
    , indexModel : UserManagement.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = UserManagement.Create.Models.initialModel
    , editModel = UserManagement.Edit.Models.initialModel ""
    , indexModel = UserManagement.Index.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        Create ->
            { model | createModel = UserManagement.Create.Models.initialModel }

        Edit uuid ->
            { model | editModel = UserManagement.Edit.Models.initialModel uuid }

        Index ->
            { model | indexModel = UserManagement.Index.Models.initialModel }
