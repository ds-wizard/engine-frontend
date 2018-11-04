module Users.Models exposing (Model, initLocalModel, initialModel)

import Users.Create.Models
import Users.Edit.Models
import Users.Index.Models
import Users.Routing exposing (Route(..))


type alias Model =
    { createModel : Users.Create.Models.Model
    , editModel : Users.Edit.Models.Model
    , indexModel : Users.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Users.Create.Models.initialModel
    , editModel = Users.Edit.Models.initialModel ""
    , indexModel = Users.Index.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        Create ->
            { model | createModel = Users.Create.Models.initialModel }

        Edit uuid ->
            { model | editModel = Users.Edit.Models.initialModel uuid }

        Index ->
            { model | indexModel = Users.Index.Models.initialModel }
