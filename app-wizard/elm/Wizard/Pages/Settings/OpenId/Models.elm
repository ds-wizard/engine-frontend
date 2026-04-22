module Wizard.Pages.Settings.OpenId.Models exposing (Model, initialModel)

import ActionResult exposing (ActionResult)
import Common.Api.Models.OpenIdClient exposing (OpenIdClient)


type alias Model =
    { openIdClients : ActionResult (List OpenIdClient)
    , openIdClientToBeDeleted : Maybe OpenIdClient
    , deletingOpenIdClient : ActionResult String
    }


initialModel : Model
initialModel =
    { openIdClients = ActionResult.Loading
    , openIdClientToBeDeleted = Nothing
    , deletingOpenIdClient = ActionResult.Unset
    }
