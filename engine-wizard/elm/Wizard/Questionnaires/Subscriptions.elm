module Wizard.Questionnaires.Subscriptions exposing (..)

import Wizard.Questionnaires.Detail.Subscriptions
import Wizard.Questionnaires.Index.Subscriptions
import Wizard.Questionnaires.Models exposing (Model)
import Wizard.Questionnaires.Msgs exposing (Msg(..))
import Wizard.Questionnaires.Routes exposing (Route(..))


subscriptions : Route -> Model -> Sub Msg
subscriptions route model =
    case route of
        DetailRoute _ ->
            Sub.map DetailMsg <| Wizard.Questionnaires.Detail.Subscriptions.subscriptions model.detailModel

        IndexRoute _ ->
            Sub.map IndexMsg <| Wizard.Questionnaires.Index.Subscriptions.subscriptions model.indexModel

        _ ->
            Sub.none
