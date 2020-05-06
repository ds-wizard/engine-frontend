module Wizard.Registry.RegistrySignupConfirmation.Msgs exposing (Msg(..))

import Shared.Error.ApiError exposing (ApiError)


type Msg
    = PostConfirmationComplete (Result ApiError ())
