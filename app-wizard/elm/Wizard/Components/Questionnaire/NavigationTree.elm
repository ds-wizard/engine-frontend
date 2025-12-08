module Wizard.Components.Questionnaire.NavigationTree exposing
    ( ViewConfig
    , view
    )

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faKmEditorTreeClosed, faKmEditorTreeOpened, faKmItemTemplate, faKmQuestion, faQuestionnaireAnsweredIndication)
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, div, i, li, span, strong, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Roman
import Set exposing (Set)
import String.Format as String
import Uuid
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Data.AppState exposing (AppState)


isOpen : List String -> Set String -> Bool
isOpen path collapsedItems =
    not (Set.member (pathToString path) collapsedItems)


type alias ViewConfig msg =
    { activeChapterUuid : Maybe String
    , nonDesirableQuestions : Bool
    , questionnaire : ProjectQuestionnaire
    , openChapter : String -> msg
    , scrollToPath : String -> msg
    , collapseItem : String -> msg
    , expandItem : String -> msg
    , collapsedItems : Set String
    }


view : AppState -> ViewConfig msg -> Html msg
view appState cfg =
    let
        chapters =
            KnowledgeModel.getChapters cfg.questionnaire.knowledgeModel
    in
    div [ class "NavigationTree" ]
        [ strong [] [ text (gettext "Chapters" appState.locale) ]
        , div [ class "nav nav-pills flex-column" ]
            (List.indexedMap (viewChapter appState cfg) chapters)
        ]


viewChapter : AppState -> ViewConfig msg -> Int -> Chapter -> Html msg
viewChapter appState cfg order chapter =
    let
        chapterQuestions =
            KnowledgeModel.getChapterQuestions chapter.uuid cfg.questionnaire.knowledgeModel
                |> List.filter (isQuestionDesirable cfg)

        questionList =
            if List.isEmpty chapterQuestions || Just chapter.uuid /= cfg.activeChapterUuid then
                Html.nothing

            else
                ul [] (List.map (viewQuestion appState cfg [ chapter.uuid ]) chapterQuestions)
    in
    div []
        [ a
            [ class "nav-link"
            , classList
                [ ( "active", cfg.activeChapterUuid == Just chapter.uuid )
                ]
            , onClick (cfg.openChapter chapter.uuid)
            ]
            [ span [ class "chapter-number" ] [ text (Roman.toRomanNumber (order + 1) ++ ". ") ]
            , span [ class "chapter-name" ] [ text chapter.title ]
            , viewChapterIndication cfg.questionnaire chapter
            ]
        , questionList
        ]


viewChapterIndication : ProjectQuestionnaire -> Chapter -> Html msg
viewChapterIndication questionnaire chapter =
    let
        unanswered =
            ProjectQuestionnaire.calculateUnansweredQuestionsForChapter questionnaire chapter
    in
    if unanswered > 0 then
        Badge.light [ class "rounded-pill" ] [ text <| String.fromInt unanswered ]

    else
        faQuestionnaireAnsweredIndication


viewQuestion : AppState -> ViewConfig msg -> List String -> Question -> Html msg
viewQuestion appState cfg path question =
    let
        currentPath =
            path ++ [ Question.getUuid question ]

        questionUuid =
            Question.getUuid question

        ( isQuestionTreeOpen, collapsePath, followUpQuestionsOrItems ) =
            case question of
                Question.OptionsQuestion _ _ ->
                    let
                        answerPath =
                            currentPath ++ [ Maybe.withDefault "" selectedAnswerUuid ]

                        selectedAnswerUuid =
                            Dict.get (pathToString currentPath) cfg.questionnaire.replies
                                |> Maybe.map (.value >> ReplyValue.getAnswerUuid)
                    in
                    ( isOpen answerPath cfg.collapsedItems
                    , answerPath
                    , viewOptionsQuestionFollowUps appState cfg questionUuid selectedAnswerUuid currentPath
                    )

                Question.ListQuestion _ _ ->
                    ( isOpen currentPath cfg.collapsedItems
                    , currentPath
                    , viewListQuestionItems appState cfg questionUuid currentPath
                    )

                _ ->
                    ( False, currentPath, Nothing )

        caret =
            viewCaret
                { hasItems = Maybe.isJust followUpQuestionsOrItems
                , isOpen = isQuestionTreeOpen
                , path = collapsePath
                , openMsg = cfg.expandItem
                , closeMsg = cfg.collapseItem
                }

        nestedList =
            if isQuestionTreeOpen then
                followUpQuestionsOrItems

            else
                Nothing
    in
    li []
        [ caret
        , a [ onClick (cfg.scrollToPath (pathToString currentPath)) ]
            [ faKmQuestion
            , text (Question.getTitle question)
            ]
        , Maybe.withDefault Html.nothing nestedList
        ]


viewOptionsQuestionFollowUps : AppState -> ViewConfig msg -> String -> Maybe String -> List String -> Maybe (Html msg)
viewOptionsQuestionFollowUps appState cfg questionUuid selectedAnswerUuid currentPath =
    let
        answers =
            KnowledgeModel.getQuestionAnswers questionUuid cfg.questionnaire.knowledgeModel

        mbSelectedAnswer =
            List.find (.uuid >> Just >> (==) selectedAnswerUuid) answers

        viewFollowups answer =
            let
                followUpQuestions =
                    KnowledgeModel.getAnswerFollowupQuestions answer.uuid cfg.questionnaire.knowledgeModel
                        |> List.filter (isQuestionDesirable cfg)
            in
            if List.isEmpty followUpQuestions then
                Nothing

            else
                Just <|
                    ul [] (List.map (viewQuestion appState cfg (currentPath ++ [ answer.uuid ])) followUpQuestions)
    in
    Maybe.andThen viewFollowups mbSelectedAnswer


viewListQuestionItems : AppState -> ViewConfig msg -> String -> List String -> Maybe (Html msg)
viewListQuestionItems appState cfg questionUuid currentPath =
    let
        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid cfg.questionnaire.knowledgeModel
                |> List.filter (isQuestionDesirable cfg)

        items =
            Dict.get (pathToString currentPath) cfg.questionnaire.replies
                |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)
                |> List.indexedMap (viewListQuestionItem appState cfg itemTemplateQuestions currentPath)
    in
    if List.isEmpty items then
        Nothing

    else
        Just <| ul [] items


viewListQuestionItem : AppState -> ViewConfig msg -> List Question -> List String -> Int -> String -> Html msg
viewListQuestionItem appState cfg itemTemplateQuestions currentPath index itemUuid =
    let
        itemPath =
            currentPath ++ [ itemUuid ]

        isItemTreeOpen =
            isOpen itemPath cfg.collapsedItems

        itemQuestions =
            if List.isEmpty itemTemplateQuestions || not isItemTreeOpen then
                Html.nothing

            else
                ul [] (List.map (viewQuestion appState cfg (currentPath ++ [ itemUuid ])) itemTemplateQuestions)

        mbItemTitle =
            ProjectQuestionnaire.getItemTitle cfg.questionnaire itemPath itemTemplateQuestions

        defaultItemTitle =
            i [] [ text (String.format (gettext "Item %s" appState.locale) [ String.fromInt (index + 1) ]) ]

        itemTitle =
            Maybe.unwrap defaultItemTitle text mbItemTitle

        itemCaret =
            viewCaret
                { hasItems = not (List.isEmpty itemTemplateQuestions)
                , isOpen = isItemTreeOpen
                , path = itemPath
                , openMsg = cfg.expandItem
                , closeMsg = cfg.collapseItem
                }
    in
    li []
        [ itemCaret
        , a [ onClick (cfg.scrollToPath (pathToString (currentPath ++ [ itemUuid ]))) ]
            [ faKmItemTemplate
            , itemTitle
            ]
        , itemQuestions
        ]


type alias ViewCaretConfig msg =
    { hasItems : Bool
    , isOpen : Bool
    , path : List String
    , openMsg : String -> msg
    , closeMsg : String -> msg
    }


viewCaret : ViewCaretConfig msg -> Html msg
viewCaret cfg =
    if not cfg.hasItems then
        Html.nothing

    else if cfg.isOpen then
        a [ class "caret", onClick (cfg.closeMsg (pathToString cfg.path)) ]
            [ faKmEditorTreeOpened ]

    else
        a [ class "caret", onClick (cfg.openMsg (pathToString cfg.path)) ]
            [ faKmEditorTreeClosed ]


pathToString : List String -> String
pathToString =
    String.join "."


isQuestionDesirable : ViewConfig msg -> Question -> Bool
isQuestionDesirable cfg =
    if cfg.nonDesirableQuestions then
        always True

    else
        Question.isDesirable cfg.questionnaire.knowledgeModel.phaseUuids
            (Uuid.toString (Maybe.withDefault Uuid.nil cfg.questionnaire.phaseUuid))
