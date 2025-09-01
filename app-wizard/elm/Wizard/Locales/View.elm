module Wizard.Locales.View exposing (view)

import Html exposing (Html)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Locales.Create.View
import Wizard.Locales.Detail.View
import Wizard.Locales.Import.View
import Wizard.Locales.Index.View
import Wizard.Locales.Models exposing (Model)
import Wizard.Locales.Msgs exposing (Msg(..))
import Wizard.Locales.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute ->
            Html.map CreateMsg <|
                Wizard.Locales.Create.View.view appState model.createModel

        DetailRoute _ ->
            Html.map DetailMsg <|
                Wizard.Locales.Detail.View.view appState model.detailModel

        ImportRoute _ ->
            Html.map ImportMsg <| Wizard.Locales.Import.View.view appState model.importModel

        IndexRoute _ ->
            Html.map IndexMsg <| Wizard.Locales.Index.View.view appState model.indexModel
