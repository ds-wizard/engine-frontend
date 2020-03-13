module Wizard.Common.Questionnaire.View exposing
    ( ViewQuestionnaireConfig
    , viewQuestionnaire
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as List
import Markdown
import Maybe.Extra as Maybe
import Roman exposing (toRomanNumber)
import Shared.Error.ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lg, lgx, lx)
import String exposing (fromInt)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FormEngine.View exposing (FormRenderer, FormViewConfig, viewForm)
import Wizard.Common.Questionnaire.Models exposing (ActivePage(..), FormExtraData, Model, calculateUnansweredQuestions, getActiveChapter)
import Wizard.Common.Questionnaire.Models.QuestionnaireFeature as QuestionnaireFeature exposing (QuestionnaireFeature)
import Wizard.Common.Questionnaire.Msgs exposing (CustomFormMessage(..), Msg(..))
import Wizard.Common.Questionnaire.Views.FeedbackModal as FeedbackModal
import Wizard.Common.Questionnaire.Views.SummaryReport as SummaryReport
import Wizard.Common.Questionnaire.Views.Todos as Todos
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import Wizard.KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.KMEditor.Common.KnowledgeModel.Level exposing (Level)
import Wizard.KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import Wizard.KMEditor.Common.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Questionnaires.Common.Questionnaire as Questionnaire
import Wizard.Questionnaires.Common.QuestionnaireDetail as QuestionnaireDetail
import Wizard.Utils exposing (listInsertIf)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Questionnaire.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Questionnaire.View"


type alias ViewQuestionnaireConfig =
    { features : List QuestionnaireFeature
    , levels : Maybe (List Level)
    , getExtraQuestionClass : String -> Maybe String
    , forceDisabled : Bool
    , createRenderer : KnowledgeModel -> List Level -> List Metric -> FormRenderer CustomFormMessage Question Answer ApiError
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
            viewExtraNavigation appState cfg model
    in
    div [ class "Questionnaire" ]
        [ div [ class "chapter-list" ]
            [ level
            , chapterList appState model
            , extraActions
            ]
        , div [ id "questionnaire-body", class "questionnaire-body" ]
            (pageView appState cfg model)
        , FeedbackModal.view appState model
        ]



-- Levels selection


levelSelection : ViewQuestionnaireConfig -> AppState -> Model -> List Level -> Int -> Html Msg
levelSelection cfg appState model levels selectedLevel =
    let
        isDisabled =
            cfg.forceDisabled || (not <| QuestionnaireDetail.isEditable appState model.questionnaire)
    in
    div [ class "level-selection card bg-light" ]
        [ div [ class "card-body" ]
            [ label [] [ lgx "questionnaire.currentPhase" appState ]
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

        chapters =
            KnowledgeModel.getChapters model.questionnaire.knowledgeModel
    in
    div [ class "nav nav-pills flex-column" ]
        ([ strong [] [ lgx "chapters" appState ] ]
            ++ List.indexedMap (chapterListChapter appState model activeChapter) chapters
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
            if appState.config.features.levels.enabled then
                model.questionnaire.level

            else
                100

        unanswered =
            calculateUnansweredQuestions appState model.questionnaire.knowledgeModel effectiveLevel model.questionnaire.replies chapter
    in
    if unanswered > 0 then
        span [ class "badge badge-light badge-pill" ] [ text <| fromInt unanswered ]

    else
        faSet "questionnaire.answeredIndication" appState



-- Extra navigation


viewExtraNavigation : AppState -> ViewQuestionnaireConfig -> Model -> Html Msg
viewExtraNavigation appState cfg model =
    let
        todosLength =
            QuestionnaireDetail.todosLength model.questionnaire

        todosLinkVisible =
            QuestionnaireFeature.todoListEnabled cfg.features && todosLength > 0

        extraNavigation =
            []
                |> listInsertIf (viewTodosLink appState todosLength model.activePage) todosLinkVisible
                |> listInsertIf (viewSummaryReportLink appState model.activePage) (QuestionnaireFeature.summaryReportEnabled cfg.features)
    in
    if List.length extraNavigation > 0 then
        div [ class "nav nav-pills flex-column" ]
            ([ strong [] [ lx_ "extraNavigation.more" appState ] ]
                ++ extraNavigation
            )

    else
        emptyNode


viewSummaryReportLink : AppState -> ActivePage -> Html Msg
viewSummaryReportLink appState =
    viewLink (lg "questionnaire.summaryReport" appState) PageSummaryReport ViewSummaryReport Nothing


viewTodosLink : AppState -> Int -> ActivePage -> Html Msg
viewTodosLink appState todosCount =
    viewLink (lg "questionnaire.todos" appState) PageTodos ViewTodos (Just todosCount)


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
            [ Page.actionResultView appState (SummaryReport.view appState model) model.summaryReport ]

        PageTodos ->
            [ Todos.view appState model ]


chapterHeader : Model -> Chapter -> Html Msg
chapterHeader model chapter =
    let
        chapterNumber =
            KnowledgeModel.getChapters model.questionnaire.knowledgeModel
                |> List.indexedMap (\i c -> ( i, c ))
                |> List.find (\( _, c ) -> c.uuid == chapter.uuid)
                |> Maybe.map (\( i, _ ) -> i + 1)
                |> Maybe.withDefault 1
                |> toRomanNumber
    in
    div []
        [ h2 [] [ text <| chapterNumber ++ ". " ++ chapter.title ]
        , Markdown.toHtml [ class "chapter-description" ] (Maybe.withDefault "" chapter.text)
        ]


formConfig : AppState -> ViewQuestionnaireConfig -> Model -> FormViewConfig CustomFormMessage Question Answer ApiError
formConfig appState cfg model =
    let
        customActions =
            []
                |> listInsertIf (viewTodoAction appState model) (QuestionnaireFeature.todosEnabled cfg.features)
                |> listInsertIf (viewFeedbackAction appState) (QuestionnaireFeature.feedbackEnabled appState cfg.features)

        isDesirable =
            if Maybe.isNothing cfg.levels then
                Just <| always False

            else
                Just (Question.getRequiredLevel >> Maybe.map ((>=) model.questionnaire.level) >> Maybe.withDefault False)
    in
    { customActions = customActions
    , isDesirable = isDesirable
    , disabled = cfg.forceDisabled || (not <| QuestionnaireDetail.isEditable appState model.questionnaire)
    , getExtraQuestionClass = cfg.getExtraQuestionClass
    , renderer = cfg.createRenderer model.questionnaire.knowledgeModel (Maybe.withDefault [] cfg.levels) model.metrics
    , appState = appState
    }



-- Custom form actions


viewTodoAction : AppState -> Model -> String -> List String -> Html CustomFormMessage
viewTodoAction appState model questionId path =
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
                [ span [] [ lx_ "todoAction.todo" appState ]
                , a
                    [ title <| l_ "todoAction.removeTodo" appState
                    , onClick <| RemoveTodo currentPath
                    ]
                    [ faSet "_global.remove" appState ]
                ]

        else
            a [ class "action action-add-todo", onClick <| AddTodo currentPath ]
                [ faSet "_global.add" appState
                , span [] [ span [] [ lx_ "todoAction.addTodo" appState ] ]
                ]

    else
        emptyNode


viewFeedbackAction : AppState -> String -> List String -> Html CustomFormMessage
viewFeedbackAction appState _ _ =
    a [ class "action", onClick <| FeedbackMsg ]
        [ faSet "questionnaire.feedback" appState ]
