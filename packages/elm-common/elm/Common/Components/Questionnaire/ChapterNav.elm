module Common.Components.Questionnaire.ChapterNav exposing
    ( ChapterEntry
    , ChapterNav
    , Context
    , Entry
    , Msg(..)
    , build
    , refreshItemTitles
    , view
    )

{-| The chapter navigation: every chapter with how many of its questions are
unanswered, and inside the open one its questions, the follow-ups under the
chosen answers and the items of its lists — the same tree the content shows,
folded the same way (they share the collapsed paths).

**Built with the content, not drawn from the questionnaire.** One lazy call over
the whole questionnaire, which is a new value after every keystroke, would redraw
the open chapter's tree on every one. Instead `build` runs where `Nodes.build` does — a new chapter, a
collapse, a view setting, the phase, an answer, the items of a list — and the view
is lazy over what it built and the unanswered counts, whose `Dict` is kept when
nothing in it changed. Typing re-renders it only when the text typed is an item's
title (`refreshItemTitles`), and then the one tree.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Chapter as Chapter
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Components.Questionnaire.Path as Path
import Dict exposing (Dict)
import Gettext exposing (gettext, ngettext)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import List.Extra as List
import Set exposing (Set)
import String.Format as String
import Ui.Badge as Badge
import Ui.Icon as Icon
import Ui.Tree as Tree
import Uuid


type alias ChapterNav =
    { chapters : List ChapterEntry
    , activeChapterUuid : String

    -- Nothing when the open chapter has no questions to list, so no toggle.
    , active : Maybe { expanded : Bool, entries : List Entry }
    }


type alias ChapterEntry =
    { uuid : String
    , number : Int
    , title : String
    }


type Entry
    = QuestionEntry { path : String, title : String, branch : Branch }
    | ItemEntry { path : String, index : Int, title : Maybe String, templates : List Question, branch : Branch }


{-| Whether an entry opens, and what it opens. The string is the collapsed path
its toggle adds or removes: an options question's is the chosen answer's path,
as in the content.
-}
type Branch
    = NoBranch
    | Collapsed String
    | Expanded String (List Entry)


type alias Context =
    { questionnaire : ProjectQuestionnaire
    , chapterUuid : String
    , collapsedPaths : Set String
    , nonDesirableQuestions : Bool
    , kmLocale : Gettext.Locale
    }


build : Context -> ChapterNav
build ctx =
    let
        km =
            ctx.questionnaire.knowledgeModel

        chapters =
            KnowledgeModel.getChapters km
                |> List.indexedMap
                    (\index chapter ->
                        { uuid = chapter.uuid
                        , number = index + 1
                        , title = (Chapter.localize ctx.kmLocale chapter).title
                        }
                    )

        questions =
            KnowledgeModel.getChapterQuestions ctx.chapterUuid km
                |> List.filter (isDesirable ctx)
    in
    { chapters = chapters
    , activeChapterUuid = ctx.chapterUuid
    , active =
        if List.isEmpty questions then
            Nothing

        else
            let
                expanded =
                    isOpen ctx ctx.chapterUuid
            in
            Just
                { expanded = expanded
                , entries =
                    if expanded then
                        List.map (buildQuestion ctx [ ctx.chapterUuid ]) questions

                    else
                        []
                }
    }


isDesirable : Context -> Question -> Bool
isDesirable ctx question =
    ctx.nonDesirableQuestions
        || Question.isDesirable ctx.questionnaire.knowledgeModel.phaseUuids
            (Uuid.toString (Maybe.withDefault Uuid.nil ctx.questionnaire.phaseUuid))
            question


isOpen : Context -> String -> Bool
isOpen ctx path =
    not (Set.member path ctx.collapsedPaths)


{-| What `candidates` open into, built only when the branch is open: a folded
branch needs to know that it has children, not what they are.
-}
branch : Context -> String -> List a -> (a -> Entry) -> Branch
branch ctx togglePath candidates toEntry =
    if List.isEmpty candidates then
        NoBranch

    else if isOpen ctx togglePath then
        Expanded togglePath (List.map toEntry candidates)

    else
        Collapsed togglePath


buildQuestion : Context -> List String -> Question -> Entry
buildQuestion ctx parentPath question =
    let
        km =
            ctx.questionnaire.knowledgeModel

        questionUuid =
            Question.getUuid question

        path =
            parentPath ++ [ questionUuid ]

        pathString =
            Path.toString path

        mbValue =
            Dict.get pathString ctx.questionnaire.replies
                |> Maybe.map .value

        questionBranch =
            case question of
                Question.OptionsQuestion _ _ ->
                    case Maybe.map ReplyValue.getAnswerUuid mbValue of
                        Just answerUuid ->
                            case List.find (.uuid >> (==) answerUuid) (KnowledgeModel.getQuestionAnswers questionUuid km) of
                                Just answer ->
                                    branch ctx
                                        (Path.toString (path ++ [ answer.uuid ]))
                                        (KnowledgeModel.getAnswerFollowupQuestions answer.uuid km
                                            |> List.filter (isDesirable ctx)
                                        )
                                        (buildQuestion ctx (path ++ [ answer.uuid ]))

                                Nothing ->
                                    NoBranch

                        Nothing ->
                            NoBranch

                Question.ListQuestion _ _ ->
                    let
                        templates =
                            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km
                                |> List.filter (isDesirable ctx)
                    in
                    branch ctx
                        pathString
                        (Maybe.map ReplyValue.getItemUuids mbValue
                            |> Maybe.withDefault []
                            |> List.indexedMap Tuple.pair
                        )
                        (\( index, itemUuid ) -> buildItem ctx path templates index itemUuid)

                _ ->
                    NoBranch
    in
    QuestionEntry
        { path = pathString
        , title = Question.getTitle (Question.localize ctx.kmLocale question)
        , branch = questionBranch
        }


buildItem : Context -> List String -> List Question -> Int -> String -> Entry
buildItem ctx listPath templates index itemUuid =
    let
        itemPath =
            listPath ++ [ itemUuid ]

        itemPathString =
            Path.toString itemPath
    in
    ItemEntry
        { path = itemPathString
        , index = index
        , title = ProjectQuestionnaire.getItemTitle ctx.questionnaire itemPath templates
        , templates = templates
        , branch =
            branch ctx itemPathString templates (buildQuestion ctx itemPath)
        }


{-| The navigation with its items' titles as they are in `questionnaire` — the
very same value when none of them changed, so the lazy view above it hits.
An item's title is the answer to its first question, typed like any other, so
this runs after every reply that does not rebuild.
-}
refreshItemTitles : ProjectQuestionnaire -> ChapterNav -> ChapterNav
refreshItemTitles questionnaire nav =
    case nav.active of
        Just active ->
            let
                refreshed =
                    List.map (refreshEntry questionnaire) active.entries
            in
            if List.all identity (List.map2 (\old new -> old == new) (itemTitles active.entries) (itemTitles refreshed)) then
                nav

            else
                { nav | active = Just { active | entries = refreshed } }

        Nothing ->
            nav


refreshEntry : ProjectQuestionnaire -> Entry -> Entry
refreshEntry questionnaire entry =
    let
        refreshBranch entryBranch =
            case entryBranch of
                Expanded togglePath entries ->
                    Expanded togglePath (List.map (refreshEntry questionnaire) entries)

                _ ->
                    entryBranch
    in
    case entry of
        QuestionEntry data ->
            QuestionEntry { data | branch = refreshBranch data.branch }

        ItemEntry data ->
            ItemEntry
                { data
                    | title = ProjectQuestionnaire.getItemTitle questionnaire (String.split "." data.path) data.templates
                    , branch = refreshBranch data.branch
                }


itemTitles : List Entry -> List (Maybe String)
itemTitles =
    List.concatMap
        (\entry ->
            case entry of
                QuestionEntry data ->
                    branchTitles data.branch

                ItemEntry data ->
                    data.title :: branchTitles data.branch
        )


branchTitles : Branch -> List (Maybe String)
branchTitles entryBranch =
    case entryBranch of
        Expanded _ entries ->
            itemTitles entries

        _ ->
            []



-- VIEW


type Msg
    = OpenChapter String
    | ScrollToPath String
    | ToggleCollapsed String


{-| Lazy in the root: `Lazy.lazy3 ChapterNav.view locale nav counts`.
-}
view : Gettext.Locale -> ChapterNav -> Dict String Int -> Html Msg
view locale nav counts =
    Tree.pills
        { label = gettext "Chapters" locale
        , words =
            { expand = \label -> String.format (gettext "Show what is in %s" locale) [ label ]
            , collapse = \label -> String.format (gettext "Hide what is in %s" locale) [ label ]
            }
        }
        (List.map (viewChapter locale nav counts) nav.chapters)


viewChapter : Gettext.Locale -> ChapterNav -> Dict String Int -> ChapterEntry -> Tree.Node Msg
viewChapter locale nav counts chapter =
    let
        unanswered =
            Dict.get chapter.uuid counts
                |> Maybe.withDefault 0

        indication =
            if unanswered > 0 then
                Badge.toHtml
                    (Badge.neutral (String.fromInt unanswered)
                        |> Badge.withDescription (String.format (ngettext ( "%s unanswered question", "%s unanswered questions" ) unanswered locale) [ String.fromInt unanswered ])
                    )

            else
                span [ class "c-questionnaire-nav__answered" ]
                    [ Icon.toHtml (Icon.solid "fa-check")
                    , span [ class "ui-visually-hidden" ] [ text (gettext "All answered" locale) ]
                    ]

        entry =
            Tree.node
                { key = chapter.uuid
                , label = String.fromInt chapter.number ++ ". " ++ chapter.title
                , onSelect = OpenChapter chapter.uuid
                }
                |> Tree.withEnd [ indication ]
                |> Tree.withTestId ("questionnaire_chapter-nav_" ++ chapter.uuid)
    in
    if chapter.uuid == nav.activeChapterUuid then
        case nav.active of
            Just active ->
                entry
                    |> Tree.withCurrent True
                    |> Tree.withChildren { expanded = active.expanded, onToggle = ToggleCollapsed chapter.uuid }
                        (List.map (viewEntry locale) active.entries)

            Nothing ->
                Tree.withCurrent True entry

    else
        entry


{-| A question is its title; an item has the list mark, which is what tells it
apart from the questions around it. The wizard marked every question with an
icon too — hundreds of elements on a long chapter, rebuilt with each chapter,
saying only that a question is a question.
-}
viewEntry : Gettext.Locale -> Entry -> Tree.Node Msg
viewEntry locale entry =
    case entry of
        QuestionEntry data ->
            Tree.node { key = data.path, label = data.title, onSelect = ScrollToPath data.path }
                |> withBranch locale data.branch

        ItemEntry data ->
            let
                node =
                    case data.title of
                        Just title ->
                            Tree.node { key = data.path, label = title, onSelect = ScrollToPath data.path }

                        Nothing ->
                            Tree.node
                                { key = data.path
                                , label = String.format (gettext "Item %s" locale) [ String.fromInt (data.index + 1) ]
                                , onSelect = ScrollToPath data.path
                                }
                                |> Tree.withPlaceholderLabel
            in
            node
                |> Tree.withIcon (Icon.regular "fa-file-lines")
                |> withBranch locale data.branch


withBranch : Gettext.Locale -> Branch -> Tree.Node Msg -> Tree.Node Msg
withBranch locale entryBranch node =
    case entryBranch of
        NoBranch ->
            node

        Collapsed togglePath ->
            Tree.withChildren { expanded = False, onToggle = ToggleCollapsed togglePath } [] node

        Expanded togglePath entries ->
            Tree.withChildren { expanded = True, onToggle = ToggleCollapsed togglePath } (List.map (viewEntry locale) entries) node
