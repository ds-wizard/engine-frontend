module Wizard.Registry.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Wizard.Registry.RegistrySignupConfirmation.Models
import Wizard.Registry.Routes exposing (Route(..))


type alias Model =
    { registrySignupConfirmationModel : Wizard.Registry.RegistrySignupConfirmation.Models.Model
    }


initialModel : Model
initialModel =
    { registrySignupConfirmationModel = Wizard.Registry.RegistrySignupConfirmation.Models.initialModel
    }


initLocalModel : Route -> Model -> Model
initLocalModel route model =
    case route of
        RegistrySignupConfirmationRoute _ _ ->
            { model | registrySignupConfirmationModel = Wizard.Registry.RegistrySignupConfirmation.Models.initialModel }
