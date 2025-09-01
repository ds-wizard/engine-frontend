module Wizard.Locales.Models exposing (Model, initLocalModel, initialModel)

import Shared.Data.PaginationQueryString as PaginationQueryString
import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Create.Models
import Wizard.Locales.Detail.Models
import Wizard.Locales.Import.Models
import Wizard.Locales.Index.Models
import Wizard.Locales.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Locales.Create.Models.Model
    , detailModel : Wizard.Locales.Detail.Models.Model
    , importModel : Wizard.Locales.Import.Models.Model
    , indexModel : Wizard.Locales.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Locales.Create.Models.initialModel appState
    , detailModel = Wizard.Locales.Detail.Models.initialModel ""
    , importModel = Wizard.Locales.Import.Models.initialModel appState Nothing
    , indexModel = Wizard.Locales.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Locales.Create.Models.initialModel appState }

        DetailRoute id ->
            { model | detailModel = Wizard.Locales.Detail.Models.initialModel id }

        ImportRoute mbLocaleId ->
            { model | importModel = Wizard.Locales.Import.Models.initialModel appState mbLocaleId }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.Locales.Index.Models.initialModel paginationQueryString }
