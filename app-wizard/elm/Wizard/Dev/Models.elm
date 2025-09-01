module Wizard.Dev.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Shared.Data.PaginationQueryString as PaginationQueryString
import Uuid
import Wizard.Dev.Operations.Models
import Wizard.Dev.PersistentCommandsDetail.Models
import Wizard.Dev.PersistentCommandsIndex.Models
import Wizard.Dev.Routes exposing (Route(..))


type alias Model =
    { operationsModel : Wizard.Dev.Operations.Models.Model
    , persistentCommandsDetailModel : Wizard.Dev.PersistentCommandsDetail.Models.Model
    , persistentCommandsIndexModel : Wizard.Dev.PersistentCommandsIndex.Models.Model
    }


initialModel : Model
initialModel =
    { operationsModel = Wizard.Dev.Operations.Models.initialModel
    , persistentCommandsDetailModel = Wizard.Dev.PersistentCommandsDetail.Models.initialModel Uuid.nil
    , persistentCommandsIndexModel = Wizard.Dev.PersistentCommandsIndex.Models.initialModel PaginationQueryString.empty Nothing
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        OperationsRoute ->
            { model | operationsModel = Wizard.Dev.Operations.Models.initialModel }

        PersistentCommandsDetail uuid ->
            { model | persistentCommandsDetailModel = Wizard.Dev.PersistentCommandsDetail.Models.initialModel uuid }

        PersistentCommandsIndex paginationQueryString mbState ->
            { model | persistentCommandsIndexModel = Wizard.Dev.PersistentCommandsIndex.Models.initialModel paginationQueryString mbState }
