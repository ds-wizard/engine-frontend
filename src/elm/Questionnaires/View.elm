module Questionnaires.View exposing (..)

import Html exposing (Html)
import Msgs
import Questionnaires.Create.View
import Questionnaires.Detail.View
import Questionnaires.Index.View
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))


view : Route -> (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view route wrapMsg model =
    case route of
        Create ->
            Questionnaires.Create.View.view (wrapMsg << CreateMsg) model.createModel

        Detail uuid ->
            Questionnaires.Detail.View.view (wrapMsg << DetailMsg) model.detailModel

        Index ->
            Questionnaires.Index.View.view (wrapMsg << IndexMsg) model.indexModel
