module KMPackages.View exposing (..)

import Html exposing (Html)
import KMPackages.Detail.View
import KMPackages.Import.View
import KMPackages.Index.View
import KMPackages.Models exposing (Model)
import KMPackages.Msgs exposing (Msg(..))
import KMPackages.Routing exposing (Route(..))
import Msgs


view : Route -> (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view route wrapMsg model =
    case route of
        Detail _ _ ->
            KMPackages.Detail.View.view (wrapMsg << DetailMsg) model.detailModel

        Import ->
            KMPackages.Import.View.view (wrapMsg << ImportMsg) model.importModel

        Index ->
            KMPackages.Index.View.view (wrapMsg << IndexMsg) model.indexModel
