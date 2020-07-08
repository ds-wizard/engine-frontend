module Wizard.Settings.Registry.Msgs exposing (Msg(..))

import Form
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | ToggleRegistrySignup Bool
    | FormMsg Form.Msg
    | PostSignupComplete (Result ApiError ())
