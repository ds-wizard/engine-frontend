module Questionnaires.Subscriptions exposing (subscriptions)

import Msgs
import Questionnaires.Index.Subscriptions
import Questionnaires.Models exposing (Model)
import Questionnaires.Msgs exposing (Msg(..))
import Questionnaires.Routing exposing (Route(..))


subscriptions : (Msg -> Msgs.Msg) -> Route -> Model -> Sub Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        Index ->
            Questionnaires.Index.Subscriptions.subscriptions (wrapMsg << IndexMsg) model.indexModel

        _ ->
            Sub.none
