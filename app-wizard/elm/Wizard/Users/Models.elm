module Wizard.Users.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Shared.Data.UuidOrCurrent as UuidOrCurrent
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
    , editModel = Wizard.Users.Edit.Models.initialModel appState UuidOrCurrent.empty
    , indexModel = Wizard.Users.Index.Models.initialModel PaginationQueryString.empty Nothing
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Users.Create.Models.initialModel appState }

        EditRoute uuidOrCurrent _ ->
            { model | editModel = Wizard.Users.Edit.Models.initialModel appState uuidOrCurrent }

        IndexRoute paginationQueryString mbRoute ->
            { model | indexModel = Wizard.Users.Index.Models.initialModel paginationQueryString mbRoute }
