module KMPackages.Subscriptions exposing (..)

import KMPackages.Detail.Subscriptions
import KMPackages.Models exposing (Model)
import KMPackages.Msgs exposing (Msg(DetailMsg))
import KMPackages.Routing exposing (Route(Detail))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Route -> Model -> Sub Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        Detail _ _ ->
            KMPackages.Detail.Subscriptions.subscriptions (wrapMsg << DetailMsg) model.detailModel

        _ ->
            Sub.none
