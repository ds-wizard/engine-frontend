module Wizard.Tenants.Subscriptions exposing (subscriptions)

import Wizard.Tenants.Index.Subscriptions
import Wizard.Tenants.Models exposing (Model)
import Wizard.Tenants.Msgs exposing (Msg(..))
import Wizard.Tenants.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        IndexRoute _ _ ->
            Sub.map IndexMsg <| Wizard.Tenants.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
