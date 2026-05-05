module Wizard.Pages.KnowledgeModels.Compare.View exposing (view)

import Common.Components.Page as Page
import Gettext exposing (gettext)
import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Wizard.Components.DetailPage as DetailPage
import Wizard.Components.KMComparison as KMComparison
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Common.CompareSelectModal as CompareSelectModal
import Wizard.Pages.KnowledgeModels.Compare.Models exposing (Model)
import Wizard.Pages.KnowledgeModels.Compare.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewComparison appState model) model.initialLoading


viewComparison : AppState -> Model -> () -> Html Msg
viewComparison appState model _ =
    DetailPage.container
        [ DetailPage.header (text "Compare Knowledge Models")
            [ button
                [ class "btn btn-primary"
                , onClick (CompareSelectModalMsg CompareSelectModal.open)
                ]
                [ text (gettext "Select Knowledge Models" appState.locale) ]
            ]
        , DetailPage.content
            [ Html.map KMComparisonMsg <| KMComparison.view appState model.kmComparisonModel
            ]
        , Html.map CompareSelectModalMsg <| CompareSelectModal.view appState model.compareSelectModalModel
        ]
