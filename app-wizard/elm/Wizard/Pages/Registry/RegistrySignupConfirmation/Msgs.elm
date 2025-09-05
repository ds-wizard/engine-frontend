module Wizard.Pages.Registry.RegistrySignupConfirmation.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)


type Msg
    = PostConfirmationComplete (Result ApiError ())
