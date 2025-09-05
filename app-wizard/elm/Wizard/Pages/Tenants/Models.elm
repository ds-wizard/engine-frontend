module Wizard.Pages.Tenants.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Pages.Tenants.Create.Models
import Wizard.Pages.Tenants.Detail.Models
import Wizard.Pages.Tenants.Index.Models
import Wizard.Pages.Tenants.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Pages.Tenants.Create.Models.Model
    , detailModel : Wizard.Pages.Tenants.Detail.Models.Model
    , indexModel : Wizard.Pages.Tenants.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Wizard.Pages.Tenants.Create.Models.initialModel
    , detailModel = Wizard.Pages.Tenants.Detail.Models.initialModel Uuid.nil
    , indexModel = Wizard.Pages.Tenants.Index.Models.initialModel PaginationQueryString.empty Nothing Nothing
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Pages.Tenants.Create.Models.initialModel }

        DetailRoute uuid ->
            { model | detailModel = Wizard.Pages.Tenants.Detail.Models.initialModel uuid }

        IndexRoute paginationQueryString mbEnabled mbStates ->
            { model | indexModel = Wizard.Pages.Tenants.Index.Models.initialModel paginationQueryString mbEnabled mbStates }
