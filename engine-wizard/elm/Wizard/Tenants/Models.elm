module Wizard.Tenants.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Tenants.Create.Models
import Wizard.Tenants.Detail.Models
import Wizard.Tenants.Index.Models
import Wizard.Tenants.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Tenants.Create.Models.Model
    , detailModel : Wizard.Tenants.Detail.Models.Model
    , indexModel : Wizard.Tenants.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Wizard.Tenants.Create.Models.initialModel
    , detailModel = Wizard.Tenants.Detail.Models.initialModel Uuid.nil
    , indexModel = Wizard.Tenants.Index.Models.initialModel PaginationQueryString.empty Nothing
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Tenants.Create.Models.initialModel }

        DetailRoute uuid ->
            { model | detailModel = Wizard.Tenants.Detail.Models.initialModel uuid }

        IndexRoute paginationQueryString mbEnabled ->
            { model | indexModel = Wizard.Tenants.Index.Models.initialModel paginationQueryString mbEnabled }
