module Wizard.KnowledgeModels.ResourcePage.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import Shared.Components.FontAwesome exposing (faKmResourceCollection)
import Shared.Utils.Markdown as Markdown
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Page as Page
import Wizard.KnowledgeModels.ResourcePage.Models exposing (Model)
import Wizard.KnowledgeModels.ResourcePage.Msgs exposing (Msg)


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewResourcePage appState model) model.knowledgeModel


viewResourcePage : AppState -> Model -> KnowledgeModel -> Html Msg
viewResourcePage appState model km =
    case KnowledgeModel.getResourcePage model.resourcePageUuid km of
        Just resourcePage ->
            case KnowledgeModel.getResourceCollectionByResourcePageUuid model.resourcePageUuid km of
                Just resourceCollection ->
                    div [ class "KnowledgeModels__BookReference container-fluid container-max" ]
                        [ div [ class "bg-light rounded px-3 py-3 fs-5 my-3" ]
                            [ faKmResourceCollection
                            , text resourceCollection.title
                            ]
                        , h1 [] [ text resourcePage.title ]
                        , Markdown.toHtml [] resourcePage.content
                        ]

                Nothing ->
                    div [] [ text (gettext "Resource page does not exist" appState.locale) ]

        Nothing ->
            div [] [ text (gettext "Resource page does not exist" appState.locale) ]
