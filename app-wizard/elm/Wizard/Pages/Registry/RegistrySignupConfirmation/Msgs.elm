module Wizard.Pages.Registry.RegistrySignupConfirmation.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)


type Msg
    = PostConfirmationComplete (Result ApiError ())
