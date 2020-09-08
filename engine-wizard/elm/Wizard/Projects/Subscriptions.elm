module Wizard.Projects.Subscriptions exposing (..)

import Wizard.Projects.Detail.Subscriptions
import Wizard.Projects.Index.Subscriptions
import Wizard.Projects.Models exposing (Model)
import Wizard.Projects.Msgs exposing (Msg(..))
import Wizard.Projects.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        DetailRoute _ subroute ->
            Sub.map DetailMsg <|
                Wizard.Projects.Detail.Subscriptions.subscriptions subroute model.detailModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.Projects.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
