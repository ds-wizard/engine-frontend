module Common.Questionnaire.View exposing
    ( ViewQuestionnaireConfig
    , viewQuestionnaire
    )

import Common.ApiError exposing (ApiError)
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa)
import Common.Questionnaire.Models exposing (ActivePage(..), FormExtraData, Model, calculateUnansweredQuestions, getActiveChapter)
import Common.Questionnaire.Models.QuestionnaireFeature as QuestionnaireFeature exposing (QuestionnaireFeature)
import Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Common.Questionnaire.Views.FeedbackModal as FeedbackModal
import Common.Questionnaire.Views.SummaryReport as SummaryReport
import Common.Questionnaire.Views.Todos as Todos
import Common.View.Page as Page
import FormEngine.View exposing (FormRenderer, FormViewConfig, viewForm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import KMEditor.Common.Models.Entities exposing (Answer, Chapter, Expert, Level, Metric, Question, ResourcePageReferenceData, URLReferenceData, getQuestionRequiredLevel)
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Questionnaires.Common.Questionnaire as Questionnaire
import Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail
import Roman exposing (toRomanNumber)
import String exposing (fromInt)
import Utils exposing (listInsertIf)


type alias ViewQuestionnaireConfig =
    { features : List QuestionnaireFeature
    , levels : Maybe (List Level)
    , getExtraQuestionClass : String -> Maybe String
    , forceDisabled : Bool
    , createRenderer : List Level -> List Metric -> FormRenderer CustomFormMessage Question Answer ApiError
    }


viewQuestionnaire : ViewQuestionnaireConfig -> AppState -> Model -> Html Msg
viewQuestionnaire cfg appState model =
    let
        level =
            case cfg.levels of
                Just levels ->
                    levelSelection cfg appState model levels model.questionnaire.level

                Nothing ->
                    emptyNode

        extraActions =
            viewExtraNavigation cfg model
    in
    div [ class "Questionnaire" ]
        [ div [ class "chapter-list" ]
            [ level
            , chapterList appState model
            , extraActions
            ]
        , div [ id "questionnaire-body", class "questionnaire-body" ]
            (pageView appState cfg model)
        , FeedbackModal.view model
        ]



-- Levels selection


levelSelection : ViewQuestionnaireConfig -> AppState -> Model -> List Level -> Int -> Html Msg
levelSelection cfg appState model levels selectedLevel =
    let
        isDisabled =
            cfg.forceDisabled || (not <| Questionnaire.isEditable appState model.questionnaire)
    in
    div [ class "level-selection card bg-light" ]
        [ div [ class "card-body" ]
            [ label [] [ text "Current Phase" ]
            , select [ class "form-control", onInput SetLevel, disabled isDisabled ]
                (List.map (levelSelectionOption selectedLevel) levels)
            ]
        ]


levelSelectionOption : Int -> Level -> Html Msg
levelSelectionOption selectedLevel level =
    option [ value (fromInt level.level), selected (selectedLevel == level.level) ]
        [ text level.title ]



-- Chapter list


chapterList : AppState -> Model -> Html Msg
chapterList appState model =
    let
        activeChapter =
            case model.activePage of
                PageChapter chapter _ ->
                    Just chapter

                _ ->
                    Nothing
    in
    div [ class "nav nav-pills flex-column" ]
        ([ strong [] [ text "Chapters" ] ]
            ++ List.indexedMap (chapterListChapter appState model activeChapter) model.questionnaire.knowledgeModel.chapters
        )


chapterListChapter : AppState -> Model -> Maybe Chapter -> Int -> Chapter -> Html Msg
chapterListChapter appState model activeChapter order chapter =
    a
        [ classList [ ( "nav-link", True ), ( "active", activeChapter == Just chapter ) ]
        , onClick <| SetActiveChapter chapter
        ]
        [ span [ class "chapter-number" ] [ text <| (toRomanNumber <| order + 1) ++ ". " ]
        , span [ class "chapter-name" ] [ text chapter.title ]
        , viewChapterAnsweredIndication appState model chapter
        ]


viewChapterAnsweredIndication : AppState -> Model -> Chapter -> Html Msg
viewChapterAnsweredIndication appState model chapter =
    let
        effectiveLevel =
            if appState.config.levelsEnabled then
                model.questionnaire.level

            else
                100

        unanswered =
            calculateUnansweredQuestions appState effectiveLevel model.questionnaire.replies chapter
    in
    if unanswered > 0 then
        span [ class "badge badge-light badge-pill" ] [ text <| fromInt unanswered ]

    else
        fa "check"



-- Extra navigation


viewExtraNavigation : ViewQuestionnaireConfig -> Model -> Html Msg
viewExtraNavigation cfg model =
    let
        todosLength =
            QuestionnaireDetail.todosLength model.questionnaire

        todosLinkVisible =
            QuestionnaireFeature.todoListEnabled cfg.features && todosLength > 0

        extraNavigation =
            []
                |> listInsertIf (viewTodosLink todosLength model.activePage) todosLinkVisible
                |> listInsertIf (viewSummaryReportLink model.activePage) (QuestionnaireFeature.summaryReportEnabled cfg.features)
    in
    if List.length extraNavigation > 0 then
        div [ class "nav nav-pills flex-column" ]
            ([ strong [] [ text "More" ] ]
                ++ extraNavigation
            )

    else
        emptyNode


viewSummaryReportLink : ActivePage -> Html Msg
viewSummaryReportLink =
    viewLink "Summary Report" PageSummaryReport ViewSummaryReport Nothing


viewTodosLink : Int -> ActivePage -> Html Msg
viewTodosLink todosCount =
    viewLink "TODOs" PageTodos ViewTodos (Just todosCount)


viewLink : String -> ActivePage -> Msg -> Maybe Int -> ActivePage -> Html Msg
viewLink linkText targetPage msg mbCount activePage =
    let
        indication =
            case mbCount of
                Just count ->
                    span [ class "badge badge-light badge-pill" ]
                        [ text <| fromInt count ]

                Nothing ->
                    span [] []
    in
    a
        [ class "nav-link"
        , classList [ ( "active", activePage == targetPage ) ]
        , onClick msg
        ]
        [ text linkText
        , indication
        ]



-- Chapter page


pageView : AppState -> ViewQuestionnaireConfig -> Model -> List (Html Msg)
pageView appState cfg model =
    case model.activePage of
        PageNone ->
            [ emptyNode ]

        PageChapter chapter form ->
            [ chapterHeader model chapter
            , viewForm (formConfig appState cfg model) form |> Html.map FormMsg
            ]

        PageSummaryReport ->
            [ Page.actionResultView (SummaryReport.view model) model.summaryReport ]

        PageTodos ->
            [ Todos.view model ]


chapterHeader : Model -> Chapter -> Html Msg
chapterHeader model chapter =
    let
        chapterNumber =
            model.questionnaire.knowledgeModel.chapters
                |> List.indexedMap (\i c -> ( i, c ))
                |> List.find (\( i, c ) -> c.uuid == chapter.uuid)
                |> Maybe.map (\( i, c ) -> i + 1)
                |> Maybe.withDefault 1
                |> toRomanNumber
    in
    div []
        [ h2 [] [ text <| chapterNumber ++ ". " ++ chapter.title ]
        , Markdown.toHtml [ class "chapter-description" ] chapter.text
        ]


formConfig : AppState -> ViewQuestionnaireConfig -> Model -> FormViewConfig CustomFormMessage Question Answer ApiError
formConfig appState cfg model =
    let
        customActions =
            []
                |> listInsertIf (viewTodoAction model) (QuestionnaireFeature.todosEnabled cfg.features)
                |> listInsertIf viewFeedbackAction (QuestionnaireFeature.feedbackEnabled appState cfg.features)

        isDesirable =
            if Maybe.isNothing cfg.levels then
                Just <| always False

            else
                Just (getQuestionRequiredLevel >> Maybe.map ((>=) model.questionnaire.level) >> Maybe.withDefault False)
    in
    { customActions = customActions
    , isDesirable = isDesirable
    , disabled = cfg.forceDisabled || (not <| Questionnaire.isEditable appState model.questionnaire)
    , getExtraQuestionClass = cfg.getExtraQuestionClass
    , renderer = cfg.createRenderer (Maybe.withDefault [] cfg.levels) model.metrics
    }



-- Custom form actions


viewTodoAction : Model -> String -> List String -> Html CustomFormMessage
viewTodoAction model questionId path =
    let
        activeChapterUuid =
            Maybe.map .uuid <| getActiveChapter model

        currentPath =
            String.join "." <| [ Maybe.withDefault "" activeChapterUuid ] ++ path ++ [ questionId ]

        hasTodo =
            model.questionnaire.labels
                |> List.filter (.path >> (==) currentPath)
                |> (not << List.isEmpty)
    in
    if Maybe.isJust activeChapterUuid then
        if hasTodo then
            span [ class "action action-todo" ]
                [ span [] [ text "TODO" ]
                , a
                    [ title "Remove TODO"
                    , onClick <| RemoveTodo currentPath
                    ]
                    [ fa "times" ]
                ]

        else
            a [ class "action action-add-todo", onClick <| AddTodo currentPath ]
                [ fa "plus"
                , span [] [ span [] [ text "Add Todo" ] ]
                ]

    else
        emptyNode


viewFeedbackAction : String -> List String -> Html CustomFormMessage
viewFeedbackAction _ _ =
    a [ class "action", onClick <| FeedbackMsg ]
        [ fa "exclamation" ]
