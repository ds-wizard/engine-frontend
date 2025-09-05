module Wizard.Pages.Dev.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Common.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Pages.Dev.Operations.Models
import Wizard.Pages.Dev.PersistentCommandsDetail.Models
import Wizard.Pages.Dev.PersistentCommandsIndex.Models
import Wizard.Pages.Dev.Routes exposing (Route(..))


type alias Model =
    { operationsModel : Wizard.Pages.Dev.Operations.Models.Model
    , persistentCommandsDetailModel : Wizard.Pages.Dev.PersistentCommandsDetail.Models.Model
    , persistentCommandsIndexModel : Wizard.Pages.Dev.PersistentCommandsIndex.Models.Model
    }


initialModel : Model
initialModel =
    { operationsModel = Wizard.Pages.Dev.Operations.Models.initialModel
    , persistentCommandsDetailModel = Wizard.Pages.Dev.PersistentCommandsDetail.Models.initialModel Uuid.nil
    , persistentCommandsIndexModel = Wizard.Pages.Dev.PersistentCommandsIndex.Models.initialModel PaginationQueryString.empty Nothing
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        OperationsRoute ->
            { model | operationsModel = Wizard.Pages.Dev.Operations.Models.initialModel }

        PersistentCommandsDetail uuid ->
            { model | persistentCommandsDetailModel = Wizard.Pages.Dev.PersistentCommandsDetail.Models.initialModel uuid }

        PersistentCommandsIndex paginationQueryString mbState ->
            { model | persistentCommandsIndexModel = Wizard.Pages.Dev.PersistentCommandsIndex.Models.initialModel paginationQueryString mbState }
