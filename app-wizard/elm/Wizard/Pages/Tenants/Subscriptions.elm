module Wizard.Pages.Tenants.Subscriptions exposing (subscriptions)

import Wizard.Pages.Tenants.Index.Subscriptions
import Wizard.Pages.Tenants.Models exposing (Model)
import Wizard.Pages.Tenants.Msgs exposing (Msg(..))
import Wizard.Pages.Tenants.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        IndexRoute _ _ _ ->
            Sub.map IndexMsg <| Wizard.Pages.Tenants.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
