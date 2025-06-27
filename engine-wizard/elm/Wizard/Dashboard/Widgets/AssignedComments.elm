module Wizard.Dashboard.Widgets.AssignedComments exposing (view)

import ActionResult exposing (ActionResult(..))
import Gettext exposing (gettext)
import Html exposing (Html, div, h2, strong, text)
import Html.Attributes exposing (class)
import Shared.Html exposing (emptyNode)
import String.Format as String
import Time.Distance exposing (inWordsWithConfig)
import Wizard.Api.Models.QuestionnaireCommentThreadAssigned exposing (QuestionnaireCommentThreadAssigned)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.TimeDistance exposing (locale)
import Wizard.Common.View.ItemIcon as ItemIcon
import Wizard.Dashboard.Widgets.WidgetHelpers as WidgetHelpers
import Wizard.Routes as Routes


view : AppState -> ActionResult (List QuestionnaireCommentThreadAssigned) -> Html msg
view appState commentThreads =
    case commentThreads of
        Unset ->
            emptyNode

        Loading ->
            emptyNode

        Error error ->
            WidgetHelpers.widget <| [ WidgetHelpers.widgetError appState error ]

        Success commentThreadList ->
            if List.isEmpty commentThreadList then
                emptyNode

            else
                WidgetHelpers.widget <| viewCommentThreads appState commentThreadList


viewCommentThreads : AppState -> List QuestionnaireCommentThreadAssigned -> List (Html msg)
viewCommentThreads appState commentThread =
    [ div [ class "RecentProjectsWidget d-flex flex-column h-100" ]
        [ h2 [ class "fs-4 fw-bold mb-4" ] [ text (gettext "Unresolved Assigned Comments" appState.locale) ]
        , div [ class "Dashboard__ItemList flex-grow-1" ] (List.map (viewCommentThread appState) commentThread)
        , div [ class "mt-4" ]
            [ linkTo appState
                Routes.commentsIndex
                []
                [ text (gettext "View all" appState.locale) ]
            ]
        ]
    ]


viewCommentThread : AppState -> QuestionnaireCommentThreadAssigned -> Html msg
viewCommentThread appState commentThread =
    let
        updatedText =
            inWordsWithConfig { withAffix = True } (locale appState) commentThread.updatedAt appState.currentTime
    in
    linkTo appState
        (Routes.projectsDetailQuestionnaire commentThread.questionnaireUuid (Just commentThread.path) (Just commentThread.commentThreadUuid))
        [ class "p-2 py-3 d-flex rounded-3" ]
        [ ItemIcon.view { text = commentThread.questionnaireName, image = Nothing }
        , div [ class "ms-2 flex-grow-1 content" ]
            [ strong [] [ text commentThread.text ]
            , div [ class "d-flex align-items-center" ]
                [ div [ class "flex-grow-1 text-lighter fst-italic" ] [ text (String.format (gettext "Updated %s" appState.locale) [ updatedText ]) ]
                ]
            ]
        ]
