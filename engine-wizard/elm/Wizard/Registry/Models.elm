module Wizard.Registry.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Wizard.Registry.RegistrySignupConfirmation.Models


type alias Model =
    { registrySignupConfirmationModel : Wizard.Registry.RegistrySignupConfirmation.Models.Model
    }


initialModel : Model
initialModel =
    { registrySignupConfirmationModel = Wizard.Registry.RegistrySignupConfirmation.Models.initialModel
    }


initLocalModel : Model -> Model
initLocalModel model =
    { model | registrySignupConfirmationModel = Wizard.Registry.RegistrySignupConfirmation.Models.initialModel }
