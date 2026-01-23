module Wizard.Components.Questionnaire2.Components.NavigationTree exposing
    ( Msg
    , UpdateConfig
    , update
    , view
    )

import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faKmEditorTreeClosed, faKmEditorTreeOpened, faKmItemTemplate, faKmQuestion, faQuestionnaireAnsweredIndication)
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, i, li, span, strong, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Html.Lazy as Lazy
import List.Extra as List
import Maybe.Extra as Maybe
import Roman
import Set exposing (Set)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Components.Questionnaire2.QuestionnaireUtils exposing (pathToString)


type Msg
    = OpenChapter String
    | ScrollToPath String
    | UpdateCollapsedPaths (Set String)


type alias UpdateConfig msg =
    { openChapterCmd : String -> Cmd msg
    , scrollToPathCmd : String -> Cmd msg
    , updateCollapsedPathsCmd : Set String -> Cmd msg
    }


update : UpdateConfig msg -> Msg -> Cmd msg
update config msg =
    case msg of
        OpenChapter chapterUuid ->
            config.openChapterCmd chapterUuid

        ScrollToPath path ->
            config.scrollToPathCmd path

        UpdateCollapsedPaths collapsedPaths ->
            config.updateCollapsedPathsCmd collapsedPaths


view :
    Gettext.Locale
    -> ProjectQuestionnaire
    -> String
    -> Bool
    -> Set String
    -> Dict String Int
    -> Html Msg
view locale questionnaire activeChapterUuid nonDesirableQuestions collapsedPaths unansweredQuestions =
    Lazy.lazy6 viewLazy
        locale
        questionnaire
        activeChapterUuid
        nonDesirableQuestions
        collapsedPaths
        unansweredQuestions


viewLazy :
    Gettext.Locale
    -> ProjectQuestionnaire
    -> String
    -> Bool
    -> Set String
    -> Dict String Int
    -> Html Msg
viewLazy locale questionnaire activeChapterUuid nonDesirableQuestions collapsedPaths unansweredQuestions =
    let
        chapters =
            KnowledgeModel.getChapters questionnaire.knowledgeModel

        isQuestionDesirable_ =
            isQuestionDesirable
                { knowledgeModel = questionnaire.knowledgeModel
                , nonDesirableQuestions = nonDesirableQuestions
                , currentPhaseUuid = questionnaire.phaseUuid
                }

        viewChapterProps =
            { activeChapterUuid = activeChapterUuid
            , collapsedPaths = collapsedPaths
            , isQuestionDesirable = isQuestionDesirable_
            , locale = locale
            , nonDesirableQuestions = nonDesirableQuestions
            , questionnaire = questionnaire
            , unansweredQuestions = unansweredQuestions
            }
    in
    div [ class "questionnaireNavigation" ]
        [ strong [ class "px-3 mb-2" ] [ text (gettext "Chapters" locale) ]
        , div [ class "nav nav-pills flex-column" ]
            (List.indexedMap (viewChapter viewChapterProps) chapters)
        ]


type alias ViewChapterProps =
    { activeChapterUuid : String
    , collapsedPaths : Set String
    , isQuestionDesirable : Question -> Bool
    , locale : Gettext.Locale
    , nonDesirableQuestions : Bool
    , questionnaire : ProjectQuestionnaire
    , unansweredQuestions : Dict String Int
    }


viewChapter : ViewChapterProps -> Int -> Chapter -> Html Msg
viewChapter props order chapter =
    let
        chapterQuestions =
            KnowledgeModel.getChapterQuestions chapter.uuid props.questionnaire.knowledgeModel
                |> List.filter props.isQuestionDesirable

        questionList =
            if List.isEmpty chapterQuestions || chapter.uuid /= props.activeChapterUuid then
                Html.nothing

            else
                let
                    viewQuestionProps =
                        { collapsedPaths = props.collapsedPaths
                        , isQuestionDesirable = props.isQuestionDesirable
                        , locale = props.locale
                        , nonDesirableQuestions = props.nonDesirableQuestions
                        , questionnaire = props.questionnaire
                        }
                in
                ul [] (List.map (viewQuestion viewQuestionProps [ chapter.uuid ]) chapterQuestions)
    in
    div []
        [ a
            [ class "nav-link"
            , classList
                [ ( "active", props.activeChapterUuid == chapter.uuid )
                ]
            , onClick (OpenChapter chapter.uuid)
            ]
            [ span [ class "chapter-number" ] [ text (Roman.toRomanNumber (order + 1) ++ ". ") ]
            , span [ class "chapter-name" ] [ text chapter.title ]
            , viewChapterIndication props.unansweredQuestions chapter
            ]
        , questionList
        ]


viewChapterIndication : Dict String Int -> Chapter -> Html msg
viewChapterIndication unansweredQuestions chapter =
    let
        unanswered =
            Dict.get chapter.uuid unansweredQuestions
                |> Maybe.withDefault 0
    in
    if unanswered > 0 then
        Badge.light [ class "rounded-pill" ] [ text <| String.fromInt unanswered ]

    else
        faQuestionnaireAnsweredIndication


type alias ViewQuestionProps =
    { collapsedPaths : Set String
    , isQuestionDesirable : Question -> Bool
    , locale : Gettext.Locale
    , nonDesirableQuestions : Bool
    , questionnaire : ProjectQuestionnaire
    }


viewQuestion : ViewQuestionProps -> List String -> Question -> Html Msg
viewQuestion props path question =
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
                            Dict.get (pathToString currentPath) props.questionnaire.replies
                                |> Maybe.map (.value >> ReplyValue.getAnswerUuid)
                    in
                    ( isOpen answerPath props.collapsedPaths
                    , answerPath
                    , viewOptionsQuestionFollowUps props questionUuid selectedAnswerUuid currentPath
                    )

                Question.ListQuestion _ _ ->
                    ( isOpen currentPath props.collapsedPaths
                    , currentPath
                    , viewListQuestionItems props questionUuid currentPath
                    )

                _ ->
                    ( False, currentPath, Nothing )

        caret =
            viewCaret
                { hasItems = Maybe.isJust followUpQuestionsOrItems
                , isOpen = isQuestionTreeOpen
                , path = collapsePath
                , collapsedPaths = props.collapsedPaths
                }

        nestedList =
            if isQuestionTreeOpen then
                followUpQuestionsOrItems

            else
                Nothing
    in
    li []
        [ caret
        , a [ onClick (ScrollToPath (pathToString currentPath)) ]
            [ faKmQuestion
            , text (Question.getTitle question)
            ]
        , Maybe.withDefault Html.nothing nestedList
        ]


viewOptionsQuestionFollowUps : ViewQuestionProps -> String -> Maybe String -> List String -> Maybe (Html Msg)
viewOptionsQuestionFollowUps props questionUuid selectedAnswerUuid currentPath =
    let
        answers =
            KnowledgeModel.getQuestionAnswers questionUuid props.questionnaire.knowledgeModel

        mbSelectedAnswer =
            List.find (.uuid >> Just >> (==) selectedAnswerUuid) answers

        viewFollowups answer =
            let
                followUpQuestions =
                    KnowledgeModel.getAnswerFollowupQuestions answer.uuid props.questionnaire.knowledgeModel
                        |> List.filter props.isQuestionDesirable
            in
            if List.isEmpty followUpQuestions then
                Nothing

            else
                Just <|
                    ul [] (List.map (viewQuestion props (currentPath ++ [ answer.uuid ])) followUpQuestions)
    in
    Maybe.andThen viewFollowups mbSelectedAnswer


viewListQuestionItems : ViewQuestionProps -> String -> List String -> Maybe (Html Msg)
viewListQuestionItems props questionUuid currentPath =
    let
        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid props.questionnaire.knowledgeModel
                |> List.filter props.isQuestionDesirable

        items =
            Dict.get (pathToString currentPath) props.questionnaire.replies
                |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)
                |> List.indexedMap (viewListQuestionItem props itemTemplateQuestions currentPath)
    in
    if List.isEmpty items then
        Nothing

    else
        Just <| ul [] items


viewListQuestionItem : ViewQuestionProps -> List Question -> List String -> Int -> String -> Html Msg
viewListQuestionItem props itemTemplateQuestions currentPath index itemUuid =
    let
        itemPath =
            currentPath ++ [ itemUuid ]

        isItemTreeOpen =
            isOpen itemPath props.collapsedPaths

        itemQuestions =
            if List.isEmpty itemTemplateQuestions || not isItemTreeOpen then
                Html.nothing

            else
                ul [] (List.map (viewQuestion props (currentPath ++ [ itemUuid ])) itemTemplateQuestions)

        mbItemTitle =
            ProjectQuestionnaire.getItemTitle props.questionnaire itemPath itemTemplateQuestions

        defaultItemTitle =
            i [] [ text (String.format (gettext "Item %s" props.locale) [ String.fromInt (index + 1) ]) ]

        itemTitle =
            Maybe.unwrap defaultItemTitle text mbItemTitle

        itemCaret =
            viewCaret
                { hasItems = not (List.isEmpty itemTemplateQuestions)
                , isOpen = isItemTreeOpen
                , path = itemPath
                , collapsedPaths = props.collapsedPaths
                }
    in
    li []
        [ itemCaret
        , a [ onClick (ScrollToPath (pathToString (currentPath ++ [ itemUuid ]))) ]
            [ faKmItemTemplate
            , itemTitle
            ]
        , itemQuestions
        ]


type alias ViewCaretConfig =
    { hasItems : Bool
    , isOpen : Bool
    , path : List String
    , collapsedPaths : Set String
    }


viewCaret : ViewCaretConfig -> Html Msg
viewCaret cfg =
    if not cfg.hasItems then
        Html.nothing

    else if cfg.isOpen then
        a [ class "caret", onClick (UpdateCollapsedPaths (Set.insert (pathToString cfg.path) cfg.collapsedPaths)) ]
            [ faKmEditorTreeOpened ]

    else
        a [ class "caret", onClick (UpdateCollapsedPaths (Set.remove (pathToString cfg.path) cfg.collapsedPaths)) ]
            [ faKmEditorTreeClosed ]


type alias IsQuestionDesirableProps =
    { knowledgeModel : KnowledgeModel
    , nonDesirableQuestions : Bool
    , currentPhaseUuid : Maybe Uuid
    }


isQuestionDesirable : IsQuestionDesirableProps -> Question -> Bool
isQuestionDesirable props =
    if props.nonDesirableQuestions then
        always True

    else
        Question.isDesirable props.knowledgeModel.phaseUuids
            (Uuid.toString (Maybe.withDefault Uuid.nil props.currentPhaseUuid))


isOpen : List String -> Set String -> Bool
isOpen path collapsedItems =
    not (Set.member (pathToString path) collapsedItems)
