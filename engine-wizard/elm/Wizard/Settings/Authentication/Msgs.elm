module Wizard.Settings.Authentication.Msgs exposing (Msg(..))

import Shared.Data.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Shared.Data.Prefab exposing (Prefab)
import Shared.Error.ApiError exposing (ApiError)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetOpenIDPrefabsComplete (Result ApiError (List (Prefab EditableOpenIDServiceConfig)))
    | FillOpenIDServiceConfig Int EditableOpenIDServiceConfig
