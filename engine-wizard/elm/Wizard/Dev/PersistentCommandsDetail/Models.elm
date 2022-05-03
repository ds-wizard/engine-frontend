module Wizard.Dev.PersistentCommandsDetail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Shared.Data.PersistentCommandDetail exposing (PersistentCommandDetail)
import Uuid exposing (Uuid)


type alias Model =
    { uuid : Uuid
    , persistentCommand : ActionResult PersistentCommandDetail
    , rerunning : ActionResult String
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , persistentCommand = Loading
    , rerunning = Unset
    }
