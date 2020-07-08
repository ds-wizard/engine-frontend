module Wizard.Templates.Models exposing (Model, initLocalModel, initialModel)

import Wizard.Common.AppState exposing (AppState)
import Wizard.Templates.Detail.Models
import Wizard.Templates.Import.Models
import Wizard.Templates.Index.Models
import Wizard.Templates.Routes exposing (Route(..))


type alias Model =
    { detailModel : Wizard.Templates.Detail.Models.Model
    , importModel : Wizard.Templates.Import.Models.Model
    , indexModel : Wizard.Templates.Index.Models.Model
    }


initialModel : AppState -> Model
initialModel appState =
    { detailModel = Wizard.Templates.Detail.Models.initialModel
    , importModel = Wizard.Templates.Import.Models.initialModel appState Nothing
    , indexModel = Wizard.Templates.Index.Models.initialModel
    }


initLocalModel : Route -> AppState -> Model -> Model
initLocalModel route appState model =
    case route of
        DetailRoute _ ->
            { model | detailModel = Wizard.Templates.Detail.Models.initialModel }

        ImportRoute packageId ->
            { model | importModel = Wizard.Templates.Import.Models.initialModel appState packageId }

        IndexRoute ->
            { model | indexModel = Wizard.Templates.Index.Models.initialModel }
