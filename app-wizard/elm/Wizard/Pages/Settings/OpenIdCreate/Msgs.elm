module Wizard.Pages.Settings.OpenIdCreate.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.OpenIdClient exposing (OpenIdClient)
import Common.Api.Models.Prefab exposing (Prefab)
import Form
import Wizard.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)


type Msg
    = GetOpenIdPrefabsComplete (Result ApiError (List (Prefab OpenIdClientDetail)))
    | PostOpenIdComplete (Result ApiError OpenIdClient)
    | Cancel
    | FormMsg Form.Msg
    | FillOpenIDServiceConfig OpenIdClientDetail
    | SetAdvancedConfigExpanded Bool
