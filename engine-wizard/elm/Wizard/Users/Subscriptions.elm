module Wizard.Users.Subscriptions exposing (..)

import Wizard.Users.Index.Subscriptions
import Wizard.Users.Models exposing (Model)
import Wizard.Users.Msgs exposing (Msg(..))
import Wizard.Users.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        IndexRoute ->
            Sub.map IndexMsg <| Wizard.Users.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
