module Wizard.Admin.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Wizard.Admin.Operations.Models
import Wizard.Admin.Routes exposing (Route(..))


type alias Model =
    { operationsModel : Wizard.Admin.Operations.Models.Model }


initialModel : Model
initialModel =
    { operationsModel = Wizard.Admin.Operations.Models.initialModel }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        OperationsRoute ->
            { model | operationsModel = Wizard.Admin.Operations.Models.initialModel }
