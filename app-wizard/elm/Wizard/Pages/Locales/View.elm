module Wizard.Pages.Locales.View exposing (view)

import Html exposing (Html)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Locales.Create.View
import Wizard.Pages.Locales.Detail.View
import Wizard.Pages.Locales.Import.View
import Wizard.Pages.Locales.Index.View
import Wizard.Pages.Locales.Models exposing (Model)
import Wizard.Pages.Locales.Msgs exposing (Msg(..))
import Wizard.Pages.Locales.Routes exposing (Route(..))


view : Route -> AppState -> Model -> Html Msg
view route appState model =
    case route of
        CreateRoute ->
            Html.map CreateMsg <|
                Wizard.Pages.Locales.Create.View.view appState model.createModel

        DetailRoute _ ->
            Html.map DetailMsg <|
                Wizard.Pages.Locales.Detail.View.view appState model.detailModel

        ImportRoute _ ->
            Html.map ImportMsg <| Wizard.Pages.Locales.Import.View.view appState model.importModel

        IndexRoute _ ->
            Html.map IndexMsg <| Wizard.Pages.Locales.Index.View.view appState model.indexModel
