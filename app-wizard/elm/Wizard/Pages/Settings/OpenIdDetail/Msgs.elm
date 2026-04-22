module Wizard.Pages.Settings.OpenIdDetail.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Form
import Wizard.Api.Models.OpenIdClientDetail exposing (OpenIdClientDetail)
import Wizard.Components.CopyableCodeBlock as CopyableCodeBlock


type Msg
    = GetOpenIdComplete (Result ApiError OpenIdClientDetail)
    | PutOpenIdComplete (Result ApiError ())
    | FormMsg Form.Msg
    | CallbackUrlCodeBlockMsg CopyableCodeBlock.Msg
    | LogoutUrlCodeBlockMsg CopyableCodeBlock.Msg
    | SetAdvancedConfigExpanded Bool
