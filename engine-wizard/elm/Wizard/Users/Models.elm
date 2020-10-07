module Wizard.Users.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Common.AppState exposing (AppState)
import Wizard.Users.Create.Models
import Wizard.Users.Edit.Models
import Wizard.Users.Index.Models
import Wizard.Users.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Users.Create.Models.Model
    , editModel : Wizard.Users.Edit.Models.Model
    , indexModel : Wizard.Users.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Users.Create.Models.initialModel appState
    , editModel = Wizard.Users.Edit.Models.initialModel ""
    , indexModel = Wizard.Users.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Users.Create.Models.initialModel appState }

        EditRoute uuid ->
            { model | editModel = Wizard.Users.Edit.Models.initialModel uuid }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.Users.Index.Models.initialModel paginationQueryString }
