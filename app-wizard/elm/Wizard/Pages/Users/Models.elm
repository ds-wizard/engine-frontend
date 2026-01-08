module Wizard.Pages.Users.Models exposing (Model, initLocalModel, initialModel)

import Common.Data.PaginationQueryString as PaginationQueryString
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Users.Create.Models
import Wizard.Pages.Users.Edit.Models
import Wizard.Pages.Users.Index.Models
import Wizard.Pages.Users.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Pages.Users.Create.Models.Model
    , editModel : Wizard.Pages.Users.Edit.Models.Model
    , indexModel : Wizard.Pages.Users.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Pages.Users.Create.Models.initialModel appState
    , editModel = Wizard.Pages.Users.Edit.Models.initialModel appState UuidOrCurrent.empty
    , indexModel = Wizard.Pages.Users.Index.Models.initialModel PaginationQueryString.empty Nothing
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Pages.Users.Create.Models.initialModel appState }

        EditRoute uuidOrCurrent userEditRoute ->
            { model | editModel = Wizard.Pages.Users.Edit.Models.initLocalModel appState userEditRoute uuidOrCurrent model.editModel }

        IndexRoute paginationQueryString mbRoute ->
            { model | indexModel = Wizard.Pages.Users.Index.Models.initialModel paginationQueryString mbRoute }
