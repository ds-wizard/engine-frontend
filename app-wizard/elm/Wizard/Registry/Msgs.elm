module Wizard.Registry.Msgs exposing (Msg(..))

import Wizard.Registry.RegistrySignupConfirmation.Msgs


type Msg
    = RegistrySignupConfirmationMsg Wizard.Registry.RegistrySignupConfirmation.Msgs.Msg
