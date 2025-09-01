module Wizard.Settings.Authentication.Msgs exposing (Msg(..))

import Shared.Data.ApiError exposing (ApiError)
import Shared.Data.Prefab exposing (Prefab)
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Wizard.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetOpenIDPrefabsComplete (Result ApiError (List (Prefab EditableOpenIDServiceConfig)))
    | FillOpenIDServiceConfig Int EditableOpenIDServiceConfig
