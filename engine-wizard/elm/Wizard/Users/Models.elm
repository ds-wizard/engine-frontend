module Wizard.Users.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Users.Create.Models
import Wizard.Users.Edit.Models
import Wizard.Users.Index.Models
import Wizard.Users.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Users.Create.Models.Model
    , editModel : Wizard.Users.Edit.Models.Model
    , indexModel : Wizard.Users.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Wizard.Users.Create.Models.initialModel
    , editModel = Wizard.Users.Edit.Models.initialModel ""
    , indexModel = Wizard.Users.Index.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Users.Create.Models.initialModel }

        EditRoute uuid ->
            { model | editModel = Wizard.Users.Edit.Models.initialModel uuid }

        IndexRoute ->
            { model | indexModel = Wizard.Users.Index.Models.initialModel }
