module Wizard.Pages.Locales.Models exposing (Model, initLocalModel, initialModel)

import Common.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Locales.Create.Models
import Wizard.Pages.Locales.Detail.Models
import Wizard.Pages.Locales.Import.Models
import Wizard.Pages.Locales.Index.Models
import Wizard.Pages.Locales.Routes exposing (Route(..))


type alias Model =
    { createModel : Wizard.Pages.Locales.Create.Models.Model
    , detailModel : Wizard.Pages.Locales.Detail.Models.Model
    , importModel : Wizard.Pages.Locales.Import.Models.Model
    , indexModel : Wizard.Pages.Locales.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { createModel = Wizard.Pages.Locales.Create.Models.initialModel appState
    , detailModel = Wizard.Pages.Locales.Detail.Models.initialModel Uuid.nil
    , importModel = Wizard.Pages.Locales.Import.Models.initialModel appState Nothing
    , indexModel = Wizard.Pages.Locales.Index.Models.initialModel PaginationQueryString.empty
    }


initLocalModel : AppState -> Route -> Model -> Model
initLocalModel appState route model =
    case route of
        CreateRoute ->
            { model | createModel = Wizard.Pages.Locales.Create.Models.initialModel appState }

        DetailRoute localeUuid ->
            { model | detailModel = Wizard.Pages.Locales.Detail.Models.initialModel localeUuid }

        ImportRoute mbLocaleId ->
            { model | importModel = Wizard.Pages.Locales.Import.Models.initialModel appState mbLocaleId }

        IndexRoute paginationQueryString ->
            { model | indexModel = Wizard.Pages.Locales.Index.Models.initialModel paginationQueryString }
