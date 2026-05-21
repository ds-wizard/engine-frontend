module Wizard.Pages.Settings.OpenId.Msgs exposing (Msg(..))

import Common.Api.ApiError exposing (ApiError)
import Common.Api.Models.OpenIdClient exposing (OpenIdClient)


type Msg
    = GetOpenIdClientsComplete (Result ApiError (List OpenIdClient))
    | ShowHideDeleteOpenIdClient (Maybe OpenIdClient)
    | DeleteOpenIdClient
    | DeleteOpenIdClientComplete (Result ApiError ())
