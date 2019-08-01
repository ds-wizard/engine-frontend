module Dashboard.Widgets.DMPWorkflowWidget exposing (view)

import ActionResult exposing (ActionResult)
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Html exposing (Html, a, div, img, p, text)
import Html.Attributes exposing (class, href, src)
import Questionnaires.Common.Questionnaire exposing (Questionnaire)
import Questionnaires.Routing exposing (Route(..))
import Routing exposing (Route(..))


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
                [ text <| "Welcome to the " ++ appState.config.client.appTitle ++ "!"
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
                    [ p [] [ text "Choose a Knowledge Model suitable for your project" ]
                    ]
                , div [ class "step" ]
                    [ p [] [ text "Fill in the questionnaire, chapter by chapter" ]
                    ]
                , div [ class "step" ]
                    [ p [] [ text "Get your Data Management Plan ready to be submitted" ]
                    ]
                ]
            , div [ class "DMPWorkflowWidget__Start" ]
                [ a [ class "btn btn-primary", href <| Routing.toUrl <| Questionnaires <| Create Nothing ]
                    [ text "Start planning" ]
                ]
            ]

    else
        emptyNode
