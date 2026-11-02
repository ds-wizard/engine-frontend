module Common.Components.Questionnaire.Panel.Search exposing
    ( Model
    , Msg
    , Outcome(..)
    , Result_
    , init
    , inputId
    , update
    , view
    )

{-| Finding something in a questionnaire of thousands of questions: the answers
given, the questions themselves, their answers and choices, their references, and
the chapters.

**The scan is work, so it happens in `update`.** Typing pushes the term through a
500 ms debounce; when it settles the questionnaire is walked once and the results
are kept. Nothing here scans while drawing, and a
keystroke costs a `Dict` insert.

**What is searched is what is there**: only replies on visible paths and only
visible questions, so an answer under an option nobody chose is not offered as a
place to go. Matching is a plain case-insensitive substring, which is
what people expect from a box called Search.

**A result is a button** naming what it found, with about thirty characters either
side of the match and the match itself in a `mark`. Choosing one goes to the
question, the path or the chapter (`Outcome`).

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Common.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Common.Api.Models.KnowledgeModel.Question as Question
import Common.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue.IntegrationReplyType exposing (IntegrationReplyType(..))
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Utils.Markdown as Markdown
import Common.Utils.RegexPatterns as RegexPatterns
import Debounce exposing (Debounce)
import Dict
import Gettext exposing (Locale, gettext, ngettext)
import Html exposing (Html, button, div, li, mark, span, text, ul)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Regex
import String.Format as String
import Task
import Ui.EmptyState as EmptyState
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Text as Text


type alias Model =
    { value : String
    , debounce : Debounce String
    , term : String
    , results : List Result_
    }


{-| Where a result goes: a reply's own path, a question that has to be found
first (it may be inside something folded), or a whole chapter.
-}
type Outcome
    = GoToPath String
    | GoToQuestion String
    | GoToChapter String


type alias Result_ =
    { kind : String
    , value : String
    , outcome : Outcome
    }


init : Model
init =
    { value = "", debounce = Debounce.init, term = "", results = [] }


{-| The search box's id, for the focus the panel takes when it opens.
-}
inputId : String
inputId =
    "questionnaire-search-input"


type Msg
    = Typed String
    | DebounceMsg Debounce.Msg
    | Search String
    | Chose Outcome


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 500
    , transform = DebounceMsg
    }


update : Locale -> ProjectQuestionnaire -> Msg -> Model -> ( Model, Cmd Msg, Maybe Outcome )
update locale questionnaire msg model =
    case msg of
        Typed value ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig value model.debounce
            in
            ( { model | value = value, debounce = debounce }, cmd, Nothing )

        DebounceMsg debounceMsg ->
            let
                ( debounce, cmd ) =
                    Debounce.update debounceConfig (Debounce.takeLast searchCmd) debounceMsg model.debounce
            in
            ( { model | debounce = debounce }, cmd, Nothing )

        Search term ->
            ( { model | term = term, results = search locale questionnaire term }, Cmd.none, Nothing )

        Chose outcome ->
            ( model, Cmd.none, Just outcome )


{-| The scan, once typing has settled.
-}
searchCmd : String -> Cmd Msg
searchCmd term =
    Task.perform (always (Search term)) (Task.succeed ())



-- THE SCAN


search : Locale -> ProjectQuestionnaire -> String -> List Result_
search locale questionnaire term =
    if String.isEmpty (String.trim term) then
        []

    else
        searchReplies locale questionnaire term
            ++ searchQuestions locale questionnaire term
            ++ searchChapters locale questionnaire term


matches : String -> String -> Bool
matches term value =
    String.contains (String.toLower term) (String.toLower value)


searchReplies : Locale -> ProjectQuestionnaire -> String -> List Result_
searchReplies locale questionnaire term =
    let
        replyText : ReplyValue -> Maybe String
        replyText value =
            case value of
                ReplyValue.StringReply string ->
                    Just string

                ReplyValue.IntegrationReply (PlainType string) ->
                    Just string

                ReplyValue.IntegrationReply (IntegrationType string _) ->
                    Just (Markdown.toString string)

                ReplyValue.FileReply fileUuid ->
                    Maybe.map .fileName (ProjectQuestionnaire.getFile questionnaire fileUuid)

                _ ->
                    Nothing
    in
    Dict.toList questionnaire.replies
        |> List.filterMap
            (\( path, reply ) ->
                case replyText reply.value of
                    Just value ->
                        if matches term value && ProjectQuestionnaire.isPathVisible questionnaire path then
                            Just { kind = gettext "Answer given" locale, value = value, outcome = GoToPath path }

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )


searchQuestions : Locale -> ProjectQuestionnaire -> String -> List Result_
searchQuestions locale questionnaire term =
    let
        knowledgeModel =
            questionnaire.knowledgeModel

        questionResults _ _ question =
            let
                uuid =
                    Question.getUuid question

                found kind value =
                    { kind = kind, value = value, outcome = GoToQuestion uuid }

                title =
                    if matches term (Question.getTitle question) then
                        [ found (gettext "Question" locale) (Question.getTitle question) ]

                    else
                        case Question.getText question of
                            Just questionText ->
                                if matches term questionText then
                                    [ found (gettext "Question" locale) questionText ]

                                else
                                    []

                            Nothing ->
                                []

                answers : List Answer
                answers =
                    KnowledgeModel.getQuestionAnswers uuid knowledgeModel

                choices : List Choice
                choices =
                    KnowledgeModel.getQuestionChoices uuid knowledgeModel

                references : List Reference
                references =
                    KnowledgeModel.getQuestionReferences uuid knowledgeModel

                referenceName reference =
                    Reference.getVisibleName
                        (KnowledgeModel.getAllQuestions knowledgeModel)
                        (KnowledgeModel.getAllResourcePages knowledgeModel)
                        reference
            in
            title
                ++ List.filterMap (\answer -> keep (gettext "Answer" locale) answer.label found) answers
                ++ List.filterMap (\choice -> keep (gettext "Choice" locale) choice.label found) choices
                ++ List.filterMap (\reference -> keep (gettext "Reference" locale) (referenceName reference) found) references

        keep kind value found =
            if matches term value then
                Just (found kind value)

            else
                Nothing
    in
    ProjectQuestionnaire.concatMapVisibleQuestions questionResults questionnaire


searchChapters : Locale -> ProjectQuestionnaire -> String -> List Result_
searchChapters locale questionnaire term =
    let
        chapterResult : Chapter -> Maybe Result_
        chapterResult chapter =
            if matches term chapter.title then
                Just { kind = gettext "Chapter" locale, value = chapter.title, outcome = GoToChapter chapter.uuid }

            else
                case chapter.text of
                    Just chapterText ->
                        if matches term chapterText then
                            Just { kind = gettext "Chapter" locale, value = chapterText, outcome = GoToChapter chapter.uuid }

                        else
                            Nothing

                    Nothing ->
                        Nothing
    in
    List.filterMap chapterResult (KnowledgeModel.getChapters questionnaire.knowledgeModel)



-- VIEW


view : Locale -> Model -> Html Msg
view locale model =
    div [ class "c-questionnaire-search" ]
        [ div [ class "c-questionnaire-search__box" ]
            [ Field.toHtml
                (Field.input
                    { id = inputId
                    , label = gettext "Search in the questionnaire" locale
                    , value = model.value
                    , onInput = Typed
                    }
                    |> Field.withHiddenLabel
                    |> Field.withPlaceholder (gettext "Search in the questionnaire…" locale)
                )
            ]
        , viewResults locale model
        ]


viewResults : Locale -> Model -> Html Msg
viewResults locale model =
    if String.isEmpty (String.trim model.term) then
        text ""

    else if List.isEmpty model.results then
        div [ class "c-questionnaire-search__results c-questionnaire-search__results--message" ]
            [ Text.announcement (gettext "No results." locale)
            , EmptyState.toHtml
                (EmptyState.emptyState
                    { title = gettext "No results" locale
                    , description = String.format (gettext "Nothing in this questionnaire matches “%s”." locale) [ model.term ]
                    }
                    |> EmptyState.withIllustration (Icon.toHtml (Icon.solid "fa-magnifying-glass"))
                    |> EmptyState.withHeadingLevel 3
                )
            ]

    else
        div [ class "c-questionnaire-search__results" ]
            [ Text.announcement
                (String.format (ngettext ( "%s result.", "%s results." ) (List.length model.results) locale)
                    [ String.fromInt (List.length model.results) ]
                )
            , ul [ class "c-questionnaire-search__list" ] (List.map (viewResult model.term) model.results)
            ]


viewResult : String -> Result_ -> Html Msg
viewResult term result =
    li []
        [ button
            [ type_ "button"
            , class "c-questionnaire-search__result"
            , onClick (Chose result.outcome)
            ]
            [ span [ class "c-questionnaire-search__kind" ] [ text result.kind ]
            , span [ class "c-questionnaire-search__value" ] (highlight term result.value)
            ]
        ]


{-| The match in its surroundings: about thirty characters either side, the
windows merged when they overlap, an ellipsis where something was left out, and
the match itself in a `mark` — a highlight the page means, rather than a yellow
background.
-}
highlight : String -> String -> List (Html msg)
highlight term value =
    let
        context =
            30

        pattern =
            RegexPatterns.fromStringIC (RegexPatterns.escapeRegex term)

        windows =
            Regex.find pattern value
                |> List.map (\match -> ( match.index, match.index + String.length match.match ))
                |> merge

        piece ( start, end ) =
            let
                from =
                    max 0 (start - context)

                to =
                    min (String.length value) (end + context)
            in
            [ if from > 0 then
                text "… "

              else
                text ""
            , text (String.slice from start value)
            , mark [] [ text (String.slice start end value) ]
            , text (String.slice end to value)
            , if to < String.length value then
                text " …"

              else
                text ""
            ]
    in
    case windows of
        [] ->
            [ text value ]

        found ->
            List.concatMap piece found


{-| Overlapping matches (their contexts touching) become one window.
-}
merge : List ( Int, Int ) -> List ( Int, Int )
merge windows =
    List.foldl
        (\( start, end ) acc ->
            case acc of
                ( previousStart, previousEnd ) :: rest ->
                    if start - previousEnd <= 60 then
                        ( previousStart, end ) :: rest

                    else
                        ( start, end ) :: acc

                [] ->
                    [ ( start, end ) ]
        )
        []
        windows
        |> List.reverse
