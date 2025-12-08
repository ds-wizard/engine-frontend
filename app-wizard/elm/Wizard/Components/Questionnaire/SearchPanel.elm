module Wizard.Components.Questionnaire.SearchPanel exposing
    ( Model
    , Msg
    , SearchResult
    , SearchResultLink
    , ViewConfig
    , init
    , searchInputId
    , update
    , view
    )

import Common.Components.Flash as Flash
import Common.Utils.Markdown as Markdown
import Common.Utils.RegexPatterns as RegexPatterns
import Debounce exposing (Debounce)
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, div, input, small, span, text)
import Html.Attributes exposing (class, id, placeholder, tabindex)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extensions exposing (onKeyConfirm)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Regex
import String.Format as String
import Task.Extra as Task
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel
import Wizard.Api.Models.KnowledgeModel.Question as Question
import Wizard.Api.Models.KnowledgeModel.Reference as Reference
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Wizard.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType as IntegrationReplyType
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { searchValue : String
    , searchDebounce : Debounce String
    , searchTerm : String
    , searchResults : List SearchResult
    }


type alias SearchResult =
    { type_ : String
    , value : String
    , link : SearchResultLink
    }


type SearchResultLink
    = PathSearchResultLink String
    | QuestionSearchResultLink String
    | ChapterSearchResultLink String


init : Model
init =
    { searchValue = ""
    , searchDebounce = Debounce.init
    , searchTerm = ""
    , searchResults = []
    }


type Msg
    = SearchInput String
    | SearchDebounceMsg Debounce.Msg
    | SearchQuestionnaire String


update : AppState -> ProjectQuestionnaire -> Msg -> Model -> ( Model, Cmd Msg )
update appState questionnaire msg model =
    case msg of
        SearchInput value ->
            let
                ( debounce, debounceCmd ) =
                    Debounce.push debounceConfig value model.searchDebounce
            in
            ( { model | searchValue = value, searchDebounce = debounce }
            , debounceCmd
            )

        SearchDebounceMsg debounceMsg ->
            let
                searchQuestionnaire value =
                    Task.dispatch (SearchQuestionnaire value)

                ( updatedDebouncer, cmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast searchQuestionnaire) debounceMsg model.searchDebounce
            in
            ( { model | searchDebounce = updatedDebouncer }
            , cmd
            )

        SearchQuestionnaire value ->
            ( { model
                | searchTerm = value
                , searchResults = search appState value questionnaire
              }
            , Cmd.none
            )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 500
    , transform = SearchDebounceMsg
    }


search : AppState -> String -> ProjectQuestionnaire -> List SearchResult
search appState term questionnaire =
    searchReplies appState term questionnaire ++ searchKnowledgeModel appState term questionnaire


searchReplies : AppState -> String -> ProjectQuestionnaire -> List SearchResult
searchReplies appState term questionnaire =
    let
        tryCreateResult path value =
            if containsSearchTerm term value then
                if ProjectQuestionnaire.isPathVisible questionnaire path then
                    Just
                        { type_ = gettext "Reply" appState.locale
                        , value = value
                        , link = PathSearchResultLink path
                        }

                else
                    Nothing

            else
                Nothing

        searchReply ( path, reply ) =
            case reply.value of
                ReplyValue.StringReply str ->
                    tryCreateResult path str

                ReplyValue.IntegrationReply integrationReplyValue ->
                    case integrationReplyValue of
                        IntegrationReplyType.PlainType str ->
                            tryCreateResult path str

                        IntegrationReplyType.IntegrationType str _ ->
                            tryCreateResult path (Markdown.toString str)

                        IntegrationReplyType.IntegrationLegacyType _ str ->
                            tryCreateResult path (Markdown.toString str)

                ReplyValue.FileReply fileUuid ->
                    List.find ((==) fileUuid << .uuid) questionnaire.files
                        |> Maybe.andThen (tryCreateResult path << .fileName)

                _ ->
                    Nothing
    in
    List.filterMap searchReply (Dict.toList questionnaire.replies)


searchKnowledgeModel : AppState -> String -> ProjectQuestionnaire -> List SearchResult
searchKnowledgeModel appState term questionnaire =
    searchQuestions appState term questionnaire ++ searchChapters appState term questionnaire


searchChapters : AppState -> String -> ProjectQuestionnaire -> List SearchResult
searchChapters appState term questionnaire =
    let
        tryCreateResult chapterUuid value =
            if containsSearchTerm term value then
                Just
                    { type_ = gettext "Chapter" appState.locale
                    , value = value
                    , link = ChapterSearchResultLink chapterUuid
                    }

            else
                Nothing

        searchChapter chapter =
            tryCreateResult chapter.uuid chapter.title
                |> Maybe.orElse (tryCreateResult chapter.uuid (Maybe.withDefault "" chapter.text))
    in
    KnowledgeModel.getChapters questionnaire.knowledgeModel
        |> List.filterMap searchChapter


searchQuestions : AppState -> String -> ProjectQuestionnaire -> List SearchResult
searchQuestions appState term questionnaire =
    let
        tryCreateResult type_ questionUuid value =
            if containsSearchTerm term value then
                Just
                    { type_ = type_
                    , value = value
                    , link = QuestionSearchResultLink questionUuid
                    }

            else
                Nothing

        tryCreateQuestionResult =
            tryCreateResult (gettext "Question" appState.locale)

        tryCreateAnswerResult =
            tryCreateResult (gettext "Answer" appState.locale)

        tryCreateChoiceResult =
            tryCreateResult (gettext "Choice" appState.locale)

        tryCreateReferenceResult =
            tryCreateResult (gettext "Reference" appState.locale)

        searchQuestion _ _ question =
            let
                questionUuid =
                    Question.getUuid question

                answers =
                    KnowledgeModel.getQuestionAnswers questionUuid questionnaire.knowledgeModel

                searchAnswer answer =
                    tryCreateAnswerResult questionUuid answer.label

                choices =
                    KnowledgeModel.getQuestionChoices questionUuid questionnaire.knowledgeModel

                searchChoice choice =
                    tryCreateChoiceResult questionUuid choice.label

                references =
                    KnowledgeModel.getQuestionReferences questionUuid questionnaire.knowledgeModel

                searchReference reference =
                    tryCreateReferenceResult questionUuid
                        (Reference.getVisibleName
                            (KnowledgeModel.getAllQuestions questionnaire.knowledgeModel)
                            (KnowledgeModel.getAllResourcePages questionnaire.knowledgeModel)
                            reference
                        )

                questionResult =
                    tryCreateQuestionResult questionUuid (Question.getTitle question)
                        |> Maybe.orElse (tryCreateQuestionResult questionUuid (Maybe.unwrap "" Markdown.toString (Question.getText question)))

                results =
                    questionResult
                        :: List.map searchAnswer answers
                        ++ List.map searchChoice choices
                        ++ List.map searchReference references
            in
            Maybe.values results
    in
    ProjectQuestionnaire.concatMapVisibleQuestions searchQuestion questionnaire


containsSearchTerm : String -> String -> Bool
containsSearchTerm term str =
    String.contains (String.toLower term) (String.toLower str)


type alias ViewConfig msg =
    { scrollToPathMsg : String -> msg
    , scrollToQuestionMsg : String -> msg
    , openChapterMsg : String -> msg
    , wrapMsg : Msg -> msg
    }


searchInputId : String
searchInputId =
    "questionnaire-search-input"


view : AppState -> ViewConfig msg -> Model -> Html msg
view appState cfg model =
    let
        results =
            if List.isEmpty model.searchResults then
                Flash.info (gettext "No results" appState.locale)

            else
                div [ class "list-group list-group-flush" ] (List.map (viewResult cfg model) model.searchResults)
    in
    div []
        [ input
            [ class "form-control"
            , onInput (cfg.wrapMsg << SearchInput)
            , id searchInputId
            , placeholder (gettext "Search in questionnaire..." appState.locale)
            ]
            []
        , Html.viewIf (not (String.isEmpty model.searchTerm)) <|
            div [ class "pt-3 pb-2" ] [ text (String.format (gettext "Results for: %s" appState.locale) [ model.searchTerm ]) ]
        , Html.viewIf (not (String.isEmpty model.searchTerm)) <| results
        ]


viewResult : ViewConfig msg -> Model -> SearchResult -> Html msg
viewResult cfg model result =
    let
        onClickMsg =
            case result.link of
                PathSearchResultLink path ->
                    cfg.scrollToPathMsg path

                QuestionSearchResultLink questionUuid ->
                    cfg.scrollToQuestionMsg questionUuid

                ChapterSearchResultLink chapterUuid ->
                    cfg.openChapterMsg chapterUuid
    in
    div
        [ onClick onClickMsg
        , onKeyConfirm onClickMsg
        , class "list-group-item list-group-item-action cursor-pointer"
        , tabindex 0
        ]
        [ small [ class "text-muted" ] [ text result.type_ ]
        , highlightTermWithContext 30 model.searchTerm result.value
        ]


highlightTermWithContext : Int -> String -> String -> Html msg
highlightTermWithContext context term textContent =
    if String.isEmpty (String.trim term) then
        div [] [ text textContent ]

    else
        let
            regex =
                RegexPatterns.fromStringIC (RegexPatterns.escapeRegex term)

            matches =
                Regex.find regex textContent

            textLen =
                String.length textContent

            windows =
                matches
                    |> List.map (\m -> windowAround context m)
                    |> mergeWindows
        in
        div [] (renderWindows windows matches textContent textLen)


renderWindows : List Window -> List Regex.Match -> String -> Int -> List (Html msg)
renderWindows windows matches textContent textLen =
    let
        renderOneWindow w =
            let
                -- Only matches that intersect this window, in order
                matchesIn =
                    matches
                        |> List.filter (\m -> intersects w (matchStart m) (matchEnd m))
                        |> List.sortBy .index

                startEllipsis =
                    if w.start > 0 then
                        [ text "…" ]

                    else
                        []

                endEllipsis =
                    if w.end < textLen then
                        [ text "…" ]

                    else
                        []

                body =
                    renderWindowBody w matchesIn textContent
            in
            startEllipsis ++ body ++ endEllipsis
    in
    windows
        |> List.map renderOneWindow
        |> List.intersperse [ text " " ]
        -- small spacer between separate windows
        |> List.concat


renderWindowBody : Window -> List Regex.Match -> String -> List (Html msg)
renderWindowBody w matchesIn textContent =
    let
        step m ( acc, pos ) =
            let
                before =
                    String.slice pos (matchStart m) textContent

                highlighted =
                    spanHighlight m.match

                newPos =
                    matchEnd m
            in
            ( acc ++ [ text before, highlighted ], newPos )

        ( parts, lastPos ) =
            List.foldl step ( [], w.start ) matchesIn

        tail =
            String.slice lastPos w.end textContent
    in
    parts ++ [ text tail ]


type alias Window =
    { start : Int, end : Int }


windowAround : Int -> Regex.Match -> Window
windowAround context m =
    let
        s =
            matchStart m

        e =
            matchEnd m
    in
    { start = max 0 (s - context)
    , end = e + context
    }


mergeWindows : List Window -> List Window
mergeWindows windows =
    let
        sorted =
            List.sortBy .start windows

        merge w acc =
            case acc of
                [] ->
                    [ w ]

                last :: rest ->
                    if w.start <= last.end then
                        { start = last.start, end = max last.end w.end } :: rest

                    else
                        w :: acc
    in
    List.reverse (List.foldl merge [] sorted)


intersects : Window -> Int -> Int -> Bool
intersects w s e =
    not (e <= w.start || s >= w.end)


matchStart : Regex.Match -> Int
matchStart m =
    m.index


matchEnd : Regex.Match -> Int
matchEnd m =
    m.index + String.length m.match


spanHighlight : String -> Html msg
spanHighlight term =
    span [ class "bg-warning" ] [ text term ]
