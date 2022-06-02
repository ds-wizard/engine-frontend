module Wizard.Dashboard.Widgets.DMPWorkflowWidget exposing (view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, a, div, p, text)
import Html.Attributes exposing (class, href)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Data.Questionnaire.QuestionnaireCreation as QuestionnaireCreation
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (lf, lx)
import Shared.Undraw as Undraw
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Projects.Create.ProjectCreateRoute exposing (ProjectCreateRoute(..))
import Wizard.Projects.Routes exposing (Route(..))
import Wizard.Routes as Routes
import Wizard.Routing as Routing


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
        let
            createRoute =
                CreateRoute <|
                    if QuestionnaireCreation.fromTemplateEnabled appState.config.questionnaire.questionnaireCreation then
                        TemplateCreateRoute Nothing

                    else
                        CustomCreateRoute Nothing
        in
        div [ class "DMPWorkflowWidget", dataCy "dashboard_dmp-workflow-widget" ]
            [ div [ class "DMPWorkflowWidget__Message" ]
                [ text <| lf_ "welcome" [ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel ] appState
                ]
            , div [ class "DMPWorkflowWidget__Workflow DMPWorkflowWidget__Workflow--Images" ]
                [ div [ class "step" ]
                    [ Undraw.choice
                    ]
                , div [ class "step" ]
                    [ Undraw.setupWizard
                    ]
                , div [ class "step" ]
                    [ Undraw.upload
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
                [ a [ class "btn btn-primary", href <| Routing.toUrl appState <| Routes.ProjectsRoute createRoute ]
                    [ lx_ "startPlanning" appState ]
                ]
            ]

    else
        emptyNode
