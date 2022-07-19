module Wizard.Common.Components.Questionnaire.NavigationTree exposing
    ( Model
    , Msg
    , ViewConfig
    , initialModel
    , openChapter
    , update
    , view
    )

import Dict exposing (Dict)
import Html exposing (Html, a, div, i, li, span, strong, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Roman
import Shared.Components.Badge as Badge
import Shared.Data.KnowledgeModel as KnowledgeModel
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question)
import Shared.Data.QuestionnaireDetail as QuestionnaireDetail exposing (QuestionnaireDetail)
import Shared.Data.QuestionnaireDetail.Reply.ReplyValue as ReplyValue
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (lf, lgx)
import Shared.Markdown as Markdown
import String.Extra as String
import Wizard.Common.AppState exposing (AppState)


lf_ : String -> List String -> AppState -> String
lf_ =
    lf "Wizard.Common.Components.Questionnaire.NavigationTree"


type alias Model =
    { openPaths : Dict String Bool }


initialModel : Model
initialModel =
    { openPaths = Dict.empty }


openChapter : String -> Model -> Model
openChapter chapterUuid model =
    { model | openPaths = Dict.fromList [ ( chapterUuid, True ) ] }


isOpen : List String -> Model -> Bool
isOpen path model =
    Maybe.withDefault False <|
        Dict.get (pathToString path) model.openPaths


type Msg
    = OpenPath String
    | ClosePath String


update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenPath path ->
            { model | openPaths = Dict.insert path True model.openPaths }

        ClosePath path ->
            { model | openPaths = Dict.remove path model.openPaths }


type alias ViewConfig msg =
    { activeChapterUuid : Maybe String
    , questionnaire : QuestionnaireDetail
    , openChapter : String -> msg
    , scrollToPath : String -> msg
    , wrapMsg : Msg -> msg
    }


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    let
        chapters =
            KnowledgeModel.getChapters cfg.questionnaire.knowledgeModel
    in
    div [ class "NavigationTree" ]
        [ strong [] [ lgx "chapters" appState ]
        , div [ class "nav nav-pills flex-column" ]
            (List.indexedMap (viewChapter appState cfg model) chapters)
        ]


viewChapter : AppState -> ViewConfig msg -> Model -> Int -> Chapter -> Html msg
viewChapter appState cfg model order chapter =
    let
        currentPath =
            [ chapter.uuid ]

        chapterQuestions =
            KnowledgeModel.getChapterQuestions chapter.uuid cfg.questionnaire.knowledgeModel

        questionList =
            if List.isEmpty chapterQuestions || not (isOpen currentPath model) then
                emptyNode

            else
                ul [] (List.map (viewQuestion appState cfg model currentPath) chapterQuestions)
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
            , viewChapterIndication appState cfg.questionnaire chapter
            ]
        , questionList
        ]


viewChapterIndication : AppState -> QuestionnaireDetail -> Chapter -> Html msg
viewChapterIndication appState questionnaire chapter =
    let
        unanswered =
            QuestionnaireDetail.calculateUnansweredQuestionsForChapter appState questionnaire chapter
    in
    if unanswered > 0 then
        Badge.light [ class "rounded-pill" ] [ text <| String.fromInt unanswered ]

    else
        faSet "questionnaire.answeredIndication" appState


viewQuestion : AppState -> ViewConfig msg -> Model -> List String -> Question -> Html msg
viewQuestion appState cfg model path question =
    let
        currentPath =
            path ++ [ Question.getUuid question ]

        questionUuid =
            Question.getUuid question

        isQuestionTreeOpen =
            isOpen currentPath model

        followUpQuestionsOrItems =
            case question of
                Question.OptionsQuestion _ _ ->
                    viewOptionsQuestionFollowUps appState cfg model questionUuid currentPath

                Question.ListQuestion _ _ ->
                    viewListQuestionItems appState cfg model questionUuid currentPath

                _ ->
                    Nothing

        caret =
            viewCaret appState
                { hasItems = Maybe.isJust followUpQuestionsOrItems
                , isOpen = isQuestionTreeOpen
                , path = currentPath
                , wrapMsg = cfg.wrapMsg
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
            [ faSet "km.question" appState
            , text (Question.getTitle question)
            ]
        , Maybe.withDefault emptyNode nestedList
        ]


viewOptionsQuestionFollowUps : AppState -> ViewConfig msg -> Model -> String -> List String -> Maybe (Html msg)
viewOptionsQuestionFollowUps appState cfg model questionUuid currentPath =
    let
        answers =
            KnowledgeModel.getQuestionAnswers questionUuid cfg.questionnaire.knowledgeModel

        selectedAnswerUuid =
            Dict.get (pathToString currentPath) cfg.questionnaire.replies
                |> Maybe.map (.value >> ReplyValue.getAnswerUuid)

        mbSelectedAnswer =
            List.find (.uuid >> Just >> (==) selectedAnswerUuid) answers

        viewFollowups answer =
            let
                followUpQuestions =
                    KnowledgeModel.getAnswerFollowupQuestions answer.uuid cfg.questionnaire.knowledgeModel
            in
            if List.isEmpty followUpQuestions then
                Nothing

            else
                Just <|
                    ul [] (List.map (viewQuestion appState cfg model (currentPath ++ [ answer.uuid ])) followUpQuestions)
    in
    Maybe.andThen viewFollowups mbSelectedAnswer


viewListQuestionItems : AppState -> ViewConfig msg -> Model -> String -> List String -> Maybe (Html msg)
viewListQuestionItems appState cfg model questionUuid currentPath =
    let
        itemTemplateQuestions =
            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid cfg.questionnaire.knowledgeModel

        items =
            Dict.get (pathToString currentPath) cfg.questionnaire.replies
                |> Maybe.unwrap [] (.value >> ReplyValue.getItemUuids)
                |> List.indexedMap (viewListQuestionItem appState cfg model itemTemplateQuestions currentPath)
    in
    if List.isEmpty items then
        Nothing

    else
        Just <| ul [] items


viewListQuestionItem : AppState -> ViewConfig msg -> Model -> List Question -> List String -> Int -> String -> Html msg
viewListQuestionItem appState cfg model itemTemplateQuestions currentPath index itemUuid =
    let
        itemPath =
            currentPath ++ [ itemUuid ]

        isItemTreeOpen =
            isOpen itemPath model

        itemQuestions =
            if List.isEmpty itemTemplateQuestions || not isItemTreeOpen then
                emptyNode

            else
                ul [] (List.map (viewQuestion appState cfg model (currentPath ++ [ itemUuid ])) itemTemplateQuestions)

        firstQuestionUuid =
            Maybe.unwrap "" Question.getUuid (List.head itemTemplateQuestions)

        itemTitle =
            let
                defaultItemTitle =
                    i [] [ text (lf_ "defaultItem" [ String.fromInt (index + 1) ] appState) ]

                titleFromMarkdown value =
                    Markdown.toString value
                        |> String.split "\n"
                        |> List.find (not << String.isEmpty)

                mbAnswer =
                    Dict.get (pathToString (currentPath ++ [ itemUuid, firstQuestionUuid ])) cfg.questionnaire.replies
                        |> Maybe.andThen (.value >> ReplyValue.getStringReply >> titleFromMarkdown)
                        |> Maybe.andThen String.toMaybe
            in
            Maybe.unwrap defaultItemTitle text mbAnswer

        itemCaret =
            viewCaret appState
                { hasItems = not (List.isEmpty itemTemplateQuestions)
                , isOpen = isItemTreeOpen
                , path = itemPath
                , wrapMsg = cfg.wrapMsg
                }
    in
    li []
        [ itemCaret
        , a [ onClick (cfg.scrollToPath (pathToString (currentPath ++ [ itemUuid, firstQuestionUuid ]))) ]
            [ faSet "km.itemTemplate" appState
            , itemTitle
            ]
        , itemQuestions
        ]


type alias ViewCaretConfig msg =
    { hasItems : Bool
    , isOpen : Bool
    , path : List String
    , wrapMsg : Msg -> msg
    }


viewCaret : AppState -> ViewCaretConfig msg -> Html msg
viewCaret appState cfg =
    if not cfg.hasItems then
        emptyNode

    else if cfg.isOpen then
        a [ class "caret", onClick (cfg.wrapMsg <| ClosePath (pathToString cfg.path)) ]
            [ faSet "kmEditor.treeOpened" appState ]

    else
        a [ class "caret", onClick (cfg.wrapMsg <| OpenPath (pathToString cfg.path)) ]
            [ faSet "kmEditor.treeClosed" appState ]


pathToString : List String -> String
pathToString =
    String.join "."
