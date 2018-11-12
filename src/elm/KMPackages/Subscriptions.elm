module KMPackages.Subscriptions exposing (subscriptions)

import KMPackages.Detail.Subscriptions
import KMPackages.Import.Subscriptions
import KMPackages.Models exposing (Model)
import KMPackages.Msgs exposing (Msg(..))
import KMPackages.Routing exposing (Route(..))
import Msgs


subscriptions : (Msg -> Msgs.Msg) -> Route -> Model -> Sub Msgs.Msg
subscriptions wrapMsg route model =
    case route of
        Detail _ _ ->
            KMPackages.Detail.Subscriptions.subscriptions (wrapMsg << DetailMsg) model.detailModel

        Import ->
            KMPackages.Import.Subscriptions.subscriptions (wrapMsg << ImportMsg) model.importModel

        _ ->
            Sub.none
