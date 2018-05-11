module DSPlanner.View exposing (..)

import DSPlanner.Create.View
import DSPlanner.Detail.View
import DSPlanner.Index.View
import DSPlanner.Models exposing (Model)
import DSPlanner.Msgs exposing (Msg(..))
import DSPlanner.Routing exposing (Route(..))
import Html exposing (Html)
import Msgs


view : Route -> (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view route wrapMsg model =
    case route of
        Create ->
            DSPlanner.Create.View.view (wrapMsg << CreateMsg) model.createModel

        Detail uuid ->
            DSPlanner.Detail.View.view (wrapMsg << DetailMsg) model.detailModel

        Index ->
            DSPlanner.Index.View.view (wrapMsg << IndexMsg) model.indexModel
