module Wizard.Pages.Settings.Registry.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form
import Wizard.Pages.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | ToggleRegistrySignup Bool
    | FormMsg Form.Msg
    | PostSignupComplete (Result ApiError ())
