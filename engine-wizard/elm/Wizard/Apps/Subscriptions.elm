module Wizard.Apps.Subscriptions exposing (subscriptions)

import Wizard.Apps.Index.Subscriptions
import Wizard.Apps.Models exposing (Model)
import Wizard.Apps.Msgs exposing (Msg(..))
import Wizard.Apps.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        IndexRoute _ _ ->
            Sub.map IndexMsg <| Wizard.Apps.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
