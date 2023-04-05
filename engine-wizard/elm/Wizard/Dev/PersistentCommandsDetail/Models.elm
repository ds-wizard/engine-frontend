module Wizard.Dev.PersistentCommandsDetail.Models exposing
    ( Model
    , initialModel
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Dropdown as Dropdown
import Shared.Data.PersistentCommandDetail exposing (PersistentCommandDetail)
import Uuid exposing (Uuid)


type alias Model =
    { uuid : Uuid
    , persistentCommand : ActionResult PersistentCommandDetail
    , updating : ActionResult String
    , dropdownState : Dropdown.State
    }


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid
    , persistentCommand = Loading
    , updating = Unset
    , dropdownState = Dropdown.initialState
    }
