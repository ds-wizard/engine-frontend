module Wizard.Pages.Settings.Authentication.Msgs exposing (Msg(..))

import Common.Data.ApiError exposing (ApiError)
import Common.Data.Prefab exposing (Prefab)
import Wizard.Api.Models.EditableConfig.EditableAuthenticationConfig.EditableOpenIDServiceConfig exposing (EditableOpenIDServiceConfig)
import Wizard.Pages.Settings.Generic.Msgs as GenericMsgs


type Msg
    = GenericMsg GenericMsgs.Msg
    | GetOpenIDPrefabsComplete (Result ApiError (List (Prefab EditableOpenIDServiceConfig)))
    | FillOpenIDServiceConfig Int EditableOpenIDServiceConfig
