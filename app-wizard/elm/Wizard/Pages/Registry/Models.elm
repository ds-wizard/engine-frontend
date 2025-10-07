module Wizard.Pages.Registry.Models exposing
    ( Model
    , initLocalModel
    , initialModel
    )

import Wizard.Pages.Registry.RegistrySignupConfirmation.Models


type alias Model =
    { registrySignupConfirmationModel : Wizard.Pages.Registry.RegistrySignupConfirmation.Models.Model
    }


initialModel : Model
initialModel =
    { registrySignupConfirmationModel = Wizard.Pages.Registry.RegistrySignupConfirmation.Models.initialModel
    }


initLocalModel : Model -> Model
initLocalModel model =
    { model | registrySignupConfirmationModel = Wizard.Pages.Registry.RegistrySignupConfirmation.Models.initialModel }
