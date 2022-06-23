module Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget exposing (view)

import ActionResult exposing (ActionResult)
import Html exposing (Html, code, div, h3, span, text)
import Html.Attributes exposing (class)
import Shared.Data.Questionnaire exposing (Questionnaire)
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l, lx)
import Version
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.SimpleList as SimpleList
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.Page as Page
import Wizard.Dashboard.Msgs exposing (Msg)
import Wizard.Projects.Common.View exposing (visibilityIcons)
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Dashboard.Widgets.PhaseQuestionnaireWidget"


view : AppState -> ActionResult (List Questionnaire) -> Html Msg
view appState questionnaires =
    Page.actionResultView appState (viewQuestionnaires appState) questionnaires


viewQuestionnaires : AppState -> List Questionnaire -> Html Msg
viewQuestionnaires appState questionnaires =
    if List.length questionnaires > 0 then
        div [ class "PhaseQuestionnaireWidget" ]
            [ h3 [] [ lx_ "recentProjects" appState ]
            , viewQuestionnaireListing appState questionnaires
            ]

    else
        emptyNode


viewQuestionnaireListing : AppState -> List Questionnaire -> Html Msg
viewQuestionnaireListing appState questionnaires =
    if List.length questionnaires > 0 then
        SimpleList.view appState (simpleListConfig appState) <| List.take 10 questionnaires

    else
        div [ class "empty" ]
            [ lx_ "noProjectsInPhase" appState ]


simpleListConfig : AppState -> SimpleList.Config Questionnaire Msg
simpleListConfig appState =
    { title = listingTitle appState
    , description = listingDescription appState
    , textTitle = .name
    , emptyText = l_ "clickCreate" appState
    , updated =
        Just
            { getTime = .updatedAt
            , currentTime = appState.currentTime
            }
    , iconView = Nothing
    }


listingTitle : AppState -> Questionnaire -> Html msg
listingTitle appState questionnaire =
    span []
        (linkTo appState (Routes.projectsDetailQuestionnaire questionnaire.uuid) [] [ text questionnaire.name ]
            :: visibilityIcons appState questionnaire
        )


listingDescription : AppState -> Questionnaire -> Html msg
listingDescription appState questionnaire =
    linkTo appState
        (Routes.knowledgeModelsDetail questionnaire.package.id)
        []
        [ text questionnaire.package.name
        , text ", "
        , text <| Version.toString questionnaire.package.version
        , text " ("
        , code [] [ text questionnaire.package.id ]
        , text ")"
        ]
