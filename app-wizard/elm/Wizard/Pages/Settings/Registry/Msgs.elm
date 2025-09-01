module Wizard.Pages.Settings.Registry.Msgs exposing (Msg(..))

import Form
import Shared.Data.ApiError exposing (ApiError)
import Wizard.Pages.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | ToggleRegistrySignup Bool
    | FormMsg Form.Msg
    | PostSignupComplete (Result ApiError ())
