module Wizard.Documents.Subscriptions exposing (..)

import Wizard.Documents.Index.Subscriptions
import Wizard.Documents.Models exposing (Model)
import Wizard.Documents.Msgs exposing (Msg(..))
import Wizard.Documents.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.Documents.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
