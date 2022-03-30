module Wizard.Apps.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Apps.Create.Models
import Wizard.Apps.Detail.Models
import Wizard.Apps.Index.Models
import Wizard.Apps.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Apps.Create.Models.Model
    , detailModel : Wizard.Apps.Detail.Models.Model
    , indexModel : Wizard.Apps.Index.Models.Model
    }


initialModel : Model
initialModel =
    { createModel = Wizard.Apps.Create.Models.initialModel
    , detailModel = Wizard.Apps.Detail.Models.initialModel Uuid.nil
    , indexModel = Wizard.Apps.Index.Models.initialModel PaginationQueryString.empty Nothing
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Apps.Create.Models.initialModel }

        DetailRoute uuid ->
            { model | detailModel = Wizard.Apps.Detail.Models.initialModel uuid }

        IndexRoute paginationQueryString mbEnabled ->
            { model | indexModel = Wizard.Apps.Index.Models.initialModel paginationQueryString mbEnabled }
