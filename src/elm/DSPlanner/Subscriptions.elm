module DSPlanner.Subscriptions exposing (..)

import DSPlanner.Index.Subscriptions
import DSPlanner.Models exposing (Model)
import DSPlanner.Msgs exposing (Msg(IndexMsg))
import DSPlanner.Routing exposing (Route(Index))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Route -> Model -> Sub Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        Index ->
            DSPlanner.Index.Subscriptions.subscriptions (wrapMsg << IndexMsg) model.indexModel

        _ ->
            Sub.none
