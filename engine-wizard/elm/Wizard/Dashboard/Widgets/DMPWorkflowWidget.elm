module Wizard.Dashboard.Widgets.DMPWorkflowWidget exposing (view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, a, div, img, p, text)
import Html.Attributes exposing (class, href, src)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lf, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Questionnaires.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing


l_ : String -> AppState -> String
l_ =
    l "Wizard.Dashboard.Widgets.DMPWorkflowWidget"


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Dashboard.Widgets.DMPWorkflowWidget"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Dashboard.Widgets.DMPWorkflowWidget"


view : AppState -> ActionResult (List Questionnaire) -> Html msg
view appState questionnaires =
    let
        visible =
            questionnaires
                |> ActionResult.map (List.length >> (==) 0)
                |> ActionResult.withDefault False
    in
    if visible then
        div [ class "DMPWorkflowWidget" ]
            [ div [ class "DMPWorkflowWidget__Message" ]
                [ text <| lf_ "welcome" [ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel ] appState
                ]
            , div [ class "DMPWorkflowWidget__Workflow DMPWorkflowWidget__Workflow--Images" ]
                [ div [ class "step" ]
                    [ img [ src "/img/illustrations/undraw_choice.svg" ] []
                    ]
                , div [ class "step" ]
                    [ img [ src "/img/illustrations/undraw_setup_wizard.svg" ] []
                    ]
                , div [ class "step" ]
                    [ img [ src "/img/illustrations/undraw_upload.svg" ] []
                    ]
                ]
            , div [ class "DMPWorkflowWidget__Workflow DMPWorkflowWidget__Workflow--Texts" ]
                [ div [ class "step" ]
                    [ p [] [ lx_ "steps.chooseKM" appState ]
                    ]
                , div [ class "step" ]
                    [ p [] [ lx_ "steps.fillQuestionnaire" appState ]
                    ]
                , div [ class "step" ]
                    [ p [] [ lx_ "steps.getDMP" appState ]
                    ]
                ]
            , div [ class "DMPWorkflowWidget__Start" ]
                [ a [ class "btn btn-primary", href <| Routing.toUrl appState <| Routes.QuestionnairesRoute <| CreateRoute Nothing ]
                    [ lx_ "startPlanning" appState ]
                ]
            ]

    else
        emptyNode
