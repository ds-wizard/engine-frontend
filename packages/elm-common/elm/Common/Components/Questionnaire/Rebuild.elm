module Common.Components.Questionnaire.Rebuild exposing
    ( chapterUnansweredCounts
    , needsRebuild
    , updateChapterUnansweredCounts
    , updateCommentsCount
    , updateTodos
    , updateUnanswered
    , updateWarnings
    )

{-| What an event costs: whether the chapter's nodes are built again, and which
derived numbers are worked out again and how much of them.

**Rebuild only on structure.** `Nodes.build` makes a new record for every node,
and a new record misses its lazy check, so a rebuild re-renders the chapter. It is
needed only when the shape of the chapter can have changed: an answer chosen
(follow-ups come and go), the items of a list, a cleared reply. Typing,
checkboxes, selects, files, TODOs and comments change a reply or a count, and
only the node that shows it re-renders. Chapter changes, collapses, view settings
and the phase rebuild too, but they are not events; the questionnaire rebuilds
for them where it handles them.

**Derived numbers, incrementally.** The wizard counted the unanswered questions
of every chapter after every reply — on every keystroke. Here only the chapter
the reply is in is counted again, and only when the reply can have changed
whether its question counts as answered: an empty value becoming a value or back,
a chosen answer, a list's items, a clear. When the count comes out the same, the
very same `Dict` is returned, so whatever takes it as a lazy argument (the chapter
navigation, the toolbar) does not re-render either.

**The toolbar's other numbers** follow the same idea. TODOs change with the
labels and with what is visible (an answer, the items, a clear). Warnings are the
reason for the care: the wizard walked the whole questionnaire for them after every
keystroke in a text field. Here a reply that changes no structure checks its own
question only, and the list is worked out again only when that question's warning
came or went — otherwise the same list is kept, and so is its length in the
toolbar's lazy call. The unresolved comment count walks the questionnaire, so it is
worked out only when what is visible changes, or the counts themselves.

Any new kind of event must say here whether it rebuilds and what it recounts.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.Project.ProjectTodo exposing (ProjectTodo)
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue as ReplyValue exposing (ReplyValue)
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire, QuestionnaireUnansweredQuestion, QuestionnaireWarning)
import Common.Components.Questionnaire.Path as Path
import Dict exposing (Dict)
import List.Extra as List


needsRebuild : ProjectEvent -> Bool
needsRebuild projectEvent =
    case projectEvent of
        ProjectEvent.SetReply reply ->
            isStructural reply.value

        ProjectEvent.ClearReply _ ->
            True

        _ ->
            False


{-| The number of unanswered questions in every chapter, from scratch — for
`init`, and for an event that can move every count at once (the phase).
-}
chapterUnansweredCounts : ProjectQuestionnaire -> Dict String Int
chapterUnansweredCounts questionnaire =
    questionnaire.knowledgeModel.chapterUuids
        |> List.map (\chapterUuid -> ( chapterUuid, ProjectQuestionnaire.calculateUnansweredQuestionsForChapter questionnaire chapterUuid ))
        |> Dict.fromList


{-| The counts after `event` took the questionnaire from `before` to `after`.
-}
updateChapterUnansweredCounts : ProjectEvent -> { before : ProjectQuestionnaire, after : ProjectQuestionnaire } -> Dict String Int -> Dict String Int
updateChapterUnansweredCounts event questionnaires counts =
    case event of
        ProjectEvent.SetPhase _ ->
            keepIfEqual counts (chapterUnansweredCounts questionnaires.after)

        ProjectEvent.SetReply reply ->
            if isStructural reply.value || wasAnswered questionnaires.before reply.path /= not (ReplyValue.isEmpty reply.value) then
                recountChapter (Path.chapterUuid reply.path) questionnaires.after counts

            else
                counts

        ProjectEvent.ClearReply reply ->
            recountChapter (Path.chapterUuid reply.path) questionnaires.after counts

        _ ->
            counts


{-| A reply that can change the shape of what is shown under its question.
-}
isStructural : ReplyValue -> Bool
isStructural value =
    case value of
        ReplyValue.AnswerReply _ ->
            True

        ReplyValue.ItemListReply _ ->
            True

        _ ->
            False


wasAnswered : ProjectQuestionnaire -> String -> Bool
wasAnswered questionnaire path =
    case Dict.get path questionnaire.replies of
        Just reply ->
            not (ReplyValue.isEmpty reply.value)

        Nothing ->
            False


recountChapter : String -> ProjectQuestionnaire -> Dict String Int -> Dict String Int
recountChapter chapterUuid questionnaire counts =
    let
        count =
            ProjectQuestionnaire.calculateUnansweredQuestionsForChapter questionnaire chapterUuid
    in
    if Dict.get chapterUuid counts == Just count then
        counts

    else
        Dict.insert chapterUuid count counts


keepIfEqual : Dict String Int -> Dict String Int -> Dict String Int
keepIfEqual old new =
    if old == new then
        old

    else
        new


{-| The unanswered questions after `event`, from `after` — but only while
something is showing them (`Just`). The list is a walk of the whole
questionnaire, which is what the wizard did after every keystroke; here the panel
that shows it asks for it when it opens and gives it up when it closes, so a
reader who is not looking at it pays nothing, and one who is gets it kept up to
date. The same list is kept when the answer changed nothing in it.
-}
updateUnanswered : ProjectEvent -> ProjectQuestionnaire -> Maybe (List QuestionnaireUnansweredQuestion) -> Maybe (List QuestionnaireUnansweredQuestion)
updateUnanswered event after current =
    case current of
        Nothing ->
            Nothing

        Just unanswered ->
            case event of
                ProjectEvent.SetReply _ ->
                    Just (keepListIfEqual unanswered (ProjectQuestionnaire.getUnansweredQuestions after))

                ProjectEvent.ClearReply _ ->
                    Just (keepListIfEqual unanswered (ProjectQuestionnaire.getUnansweredQuestions after))

                ProjectEvent.SetPhase _ ->
                    Just (keepListIfEqual unanswered (ProjectQuestionnaire.getUnansweredQuestions after))

                _ ->
                    Just unanswered


keepListIfEqual : List a -> List a -> List a
keepListIfEqual old new =
    if old == new then
        old

    else
        new


{-| The TODOs after `event`, from `after`.
-}
updateTodos : ProjectEvent -> ProjectQuestionnaire -> List ProjectTodo -> List ProjectTodo
updateTodos event after todos =
    case event of
        ProjectEvent.SetLabels _ ->
            ProjectQuestionnaire.getTodos after

        ProjectEvent.SetReply reply ->
            if isStructural reply.value then
                ProjectQuestionnaire.getTodos after

            else
                todos

        ProjectEvent.ClearReply _ ->
            ProjectQuestionnaire.getTodos after

        _ ->
            todos


{-| The warnings after `event`, from `after`: the same list unless the replied
question's warning appeared or went, or what is visible changed.
-}
updateWarnings : ProjectEvent -> ProjectQuestionnaire -> List QuestionnaireWarning -> List QuestionnaireWarning
updateWarnings event after warnings =
    case event of
        ProjectEvent.SetReply reply ->
            if isStructural reply.value then
                ProjectQuestionnaire.getWarnings after

            else if hasWarning after reply.path == List.any (.path >> (==) reply.path) warnings then
                warnings

            else
                ProjectQuestionnaire.getWarnings after

        ProjectEvent.ClearReply _ ->
            ProjectQuestionnaire.getWarnings after

        _ ->
            warnings


hasWarning : ProjectQuestionnaire -> String -> Bool
hasWarning questionnaire path =
    let
        segments =
            String.split "." path

        km =
            questionnaire.knowledgeModel

        mbQuestion =
            List.last segments
                |> Maybe.andThen (\uuid -> KnowledgeModel.getQuestion uuid km)
    in
    case ( KnowledgeModel.getChapter (Path.chapterUuid path) km, mbQuestion ) of
        ( Just chapter, Just question ) ->
            not (List.isEmpty (ProjectQuestionnaire.getQuestionWarnings questionnaire chapter segments question))

        _ ->
            False


{-| The unresolved comments shown in the toolbar, after `event`: worked out again
when what is visible changed, or for a comment event, whose counts moved.
-}
updateCommentsCount : ProjectEvent -> ProjectQuestionnaire -> Int -> Int
updateCommentsCount event after count =
    case event of
        ProjectEvent.SetReply reply ->
            if isStructural reply.value then
                ProjectQuestionnaire.commentsLength after

            else
                count

        ProjectEvent.SetLabels _ ->
            count

        ProjectEvent.SetPhase _ ->
            count

        _ ->
            ProjectQuestionnaire.commentsLength after
