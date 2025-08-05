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
import Gettext exposing (gettext)
import Html exposing (Html, a, div, i, li, span, strong, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Roman
import Shared.Components.Badge as Badge
import Shared.Components.FontAwesome exposing (faKmEditorTreeClosed, faKmEditorTreeOpened, faKmItemTemplate, faKmQuestion, faQuestionnaireAnsweredIndication)
import String.Format as String
import Uuid
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.QuestionnaireDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.QuestionnaireQuestionnaire as QuestionnaireQuestionnaire exposing (QuestionnaireQuestionnaire)
import Wizard.Common.AppState exposing (AppState)


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
    , nonDesirableQuestions : Bool
    , questionnaire : QuestionnaireQuestionnaire
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
        [ strong [] [ text (gettext "Chapters" appState.locale) ]
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
                |> List.filter (isQuestionDesirable cfg)

        questionList =
            if List.isEmpty chapterQuestions || not (isOpen currentPath model) then
                Html.nothing

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
            , viewChapterIndication cfg.questionnaire chapter
            ]
        , questionList
        ]


viewChapterIndication : QuestionnaireQuestionnaire -> Chapter -> Html msg
viewChapterIndication questionnaire chapter =
    let
        unanswered =
            QuestionnaireQuestionnaire.calculateUnansweredQuestionsForChapter questionnaire chapter
    in
    if unanswered > 0 then
        Badge.light [ class "rounded-pill" ] [ text <| String.fromInt unanswered ]

    else
        faQuestionnaireAnsweredIndication


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
            viewCaret
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
            [ faKmQuestion
            , text (Question.getTitle question)
            ]
        , Maybe.withDefault Html.nothing nestedList
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
                        |> List.filter (isQuestionDesirable cfg)
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
                |> List.filter (isQuestionDesirable cfg)

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
                Html.nothing

            else
                ul [] (List.map (viewQuestion appState cfg model (currentPath ++ [ itemUuid ])) itemTemplateQuestions)

        mbItemTitle =
            QuestionnaireQuestionnaire.getItemTitle cfg.questionnaire itemPath itemTemplateQuestions

        defaultItemTitle =
            i [] [ text (String.format (gettext "Item %s" appState.locale) [ String.fromInt (index + 1) ]) ]

        itemTitle =
            Maybe.unwrap defaultItemTitle text mbItemTitle

        itemCaret =
            viewCaret
                { hasItems = not (List.isEmpty itemTemplateQuestions)
                , isOpen = isItemTreeOpen
                , path = itemPath
                , wrapMsg = cfg.wrapMsg
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
    , wrapMsg : Msg -> msg
    }


viewCaret : ViewCaretConfig msg -> Html msg
viewCaret cfg =
    if not cfg.hasItems then
        Html.nothing

    else if cfg.isOpen then
        a [ class "caret", onClick (cfg.wrapMsg <| ClosePath (pathToString cfg.path)) ]
            [ faKmEditorTreeOpened ]

    else
        a [ class "caret", onClick (cfg.wrapMsg <| OpenPath (pathToString cfg.path)) ]
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
