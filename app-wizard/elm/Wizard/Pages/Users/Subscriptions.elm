module Wizard.Pages.Users.Subscriptions exposing (subscriptions)

import Wizard.Pages.Users.Index.Subscriptions
import Wizard.Pages.Users.Models exposing (Model)
import Wizard.Pages.Users.Msgs exposing (Msg(..))
import Wizard.Pages.Users.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        IndexRoute _ _ ->
            Sub.map IndexMsg <| Wizard.Pages.Users.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
