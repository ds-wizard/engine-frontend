module Wizard.Pages.Registry.RegistrySignupConfirmation.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)


type Msg
    = PostConfirmationComplete (Result ApiError ())
