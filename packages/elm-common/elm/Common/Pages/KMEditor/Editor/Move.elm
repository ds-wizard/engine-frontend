module Common.Pages.KMEditor.Editor.Move exposing
    ( Config
    , MovingEntity(..)
    , State
    , canMove
    , collapseAll
    , event
    , expandAll
    , movingEntity
    , open
    , setSelected
    , title
    , tree
    )

{-| Moving an entity somewhere else in the knowledge model.

Five things move — a question, an answer, a choice, a reference and an expert —
and each has its own event carrying only where it is going. Everything else is
reordered inside its parent instead, which the forms do (`Ui.ReorderableList`).

**Where it may go is the whole of the problem.** A question goes into a chapter,
a list question's items or an answer's follow-ups; an answer into another
options question; a choice into another multi-choice question; a reference or an
expert into another question. Never into itself, never into the parent it is
already in — that move is a no-op the server would still broadcast — and never
into something inside it, which would cut the branch off from the tree.

**The tree is the same `Ui.Tree` as the navigation**, with everything that
cannot be chosen disabled rather than left out: a target is found by recognizing
where it lives, so the places around it have to be there to recognize it by. The
entity being moved carries a word saying so, since it is in the tree too and
looks like any other entry.

-}

import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Api.Models.KnowledgeModel.Reference as Reference
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Ui.Badge as Badge
import Ui.Icon as Icon exposing (Icon)
import Ui.Tree as Tree
import Uuid


{-| What is being moved. The five kinds the protocol has a move event for.
-}
type MovingEntity
    = MovingQuestion
    | MovingAnswer
    | MovingChoice
    | MovingReference
    | MovingExpert


type alias State =
    { entity : MovingEntity
    , uuid : String
    , parentUuid : String

    -- Where it is going, once something has been chosen.
    , selected : Maybe String
    , openNodes : Set String
    }


{-| The dialog, opened on an entity. Everything on the way to it is open, so it
can be seen in its place before it is taken out of it.
-}
open : MovingEntity -> String -> EditorContext -> State
open entity uuid editorContext =
    { entity = entity
    , uuid = uuid
    , parentUuid = EditorContext.getParentUuid uuid editorContext
    , selected = Nothing
    , openNodes = Set.fromList editorContext.openNodeUuids
    }


setSelected : String -> State -> State
setSelected uuid state =
    { state | selected = Just uuid }


expandAll : EditorContext -> State -> State
expandAll editorContext state =
    { state | openNodes = Set.fromList (EditorContext.getAllUuids editorContext) }


collapseAll : State -> State
collapseAll state =
    { state | openNodes = Set.empty }


{-| Which entities can be moved at all. Everything else — a chapter, a metric, a
phase, a tag, an integration, a resource collection or one of its pages — lives
in exactly one place, and its order there is the form's business.
-}
movingEntity : String -> EditorContext -> Maybe MovingEntity
movingEntity uuid editorContext =
    let
        km =
            editorContext.kmEditor.knowledgeModel

        is get value =
            Maybe.map (always value) (get uuid km)
    in
    is KnowledgeModel.getQuestion MovingQuestion
        |> orElse (is KnowledgeModel.getAnswer MovingAnswer)
        |> orElse (is KnowledgeModel.getChoice MovingChoice)
        |> orElse (is KnowledgeModel.getReference MovingReference)
        |> orElse (is KnowledgeModel.getExpert MovingExpert)


{-| Whether the dialog has somewhere to send it yet.
-}
canMove : State -> Bool
canMove state =
    Maybe.isJust state.selected


{-| The event that does it.
-}
event : State -> Maybe EventContent
event state =
    Maybe.map
        (\targetUuid ->
            case state.entity of
                MovingQuestion ->
                    MoveQuestionEvent { targetUuid = targetUuid }

                MovingAnswer ->
                    MoveAnswerEvent { targetUuid = targetUuid }

                MovingChoice ->
                    MoveChoiceEvent { targetUuid = targetUuid }

                MovingReference ->
                    MoveReferenceEvent { targetUuid = targetUuid }

                MovingExpert ->
                    MoveExpertEvent { targetUuid = targetUuid }
        )
        state.selected


title : Gettext.Locale -> State -> String
title locale state =
    case state.entity of
        MovingQuestion ->
            gettext "Move question" locale

        MovingAnswer ->
            gettext "Move answer" locale

        MovingChoice ->
            gettext "Move choice" locale

        MovingReference ->
            gettext "Move reference" locale

        MovingExpert ->
            gettext "Move expert" locale



-- THE TREE OF PLACES IT COULD GO


type alias Config msg =
    { locale : Gettext.Locale
    , onSelect : String -> msg
    , onToggle : String -> Bool -> msg
    }


tree : Config msg -> EditorContext -> State -> Html msg
tree config editorContext state =
    Tree.toHtml
        { label = gettext "Where it goes" config.locale
        , words =
            { expand = \label -> gettext "Show what is in " config.locale ++ label
            , collapse = \label -> gettext "Hide what is in " config.locale ++ label
            }
        }
        (knowledgeModelNode config editorContext state
            :: List.map (chapterNode config editorContext state)
                (shown .uuid editorContext (KnowledgeModel.getChapters editorContext.kmEditor.knowledgeModel))
        )


knowledgeModelNode : Config msg -> EditorContext -> State -> Tree.Node msg
knowledgeModelNode config editorContext state =
    node config
        state
        { uuid = Uuid.toString editorContext.kmEditor.knowledgeModel.uuid
        , label = editorContext.kmEditor.name
        , icon = Icon.solid "fa-sitemap"
        , allowed = False
        , children = []
        }


chapterNode : Config msg -> EditorContext -> State -> { a | uuid : String, title : String } -> Tree.Node msg
chapterNode config editorContext state chapter =
    node config
        state
        { uuid = chapter.uuid
        , label = chapter.title
        , icon = Icon.regular "fa-file"
        , allowed = state.entity == MovingQuestion && chapter.uuid /= state.parentUuid
        , children =
            List.map (questionNode config editorContext state False)
                (shown Question.getUuid editorContext (KnowledgeModel.getChapterQuestions chapter.uuid editorContext.kmEditor.knowledgeModel))
        }


{-| `inside` says this is somewhere within the entity being moved, which is the
reason the flag is threaded through: a branch moved into itself is a branch lost.
-}
questionNode : Config msg -> EditorContext -> State -> Bool -> Question -> Tree.Node msg
questionNode config editorContext state inside question =
    let
        uuid =
            Question.getUuid question

        km =
            editorContext.kmEditor.knowledgeModel

        isSelf =
            uuid == state.uuid

        within =
            inside || isSelf

        allowed =
            not within
                && uuid
                /= state.parentUuid
                && (case state.entity of
                        MovingReference ->
                            True

                        MovingExpert ->
                            True

                        MovingAnswer ->
                            Question.isOptions question

                        MovingChoice ->
                            Question.isMultiChoice question

                        MovingQuestion ->
                            Question.isList question
                   )
    in
    node config
        state
        { uuid = uuid
        , label = Question.getTitle question
        , icon = Icon.regular "fa-comment"
        , allowed = allowed
        , children =
            List.map (answerNode config editorContext state within)
                (shown .uuid editorContext (KnowledgeModel.getQuestionAnswers uuid km))
                ++ List.map (questionNode config editorContext state within)
                    (shown Question.getUuid editorContext (KnowledgeModel.getQuestionItemTemplateQuestions uuid km))
                -- Only the one being moved, so it can be seen where it is now.
                ++ List.map (leafNode config state (Icon.regular "fa-user") .name)
                    (List.filter (\expert -> expert.uuid == state.uuid) (KnowledgeModel.getQuestionExperts uuid km))
                ++ List.map
                    (leafNode config
                        state
                        (Icon.regular "fa-bookmark")
                        (Reference.getVisibleName (KnowledgeModel.getAllQuestions km) (KnowledgeModel.getAllResourcePages km))
                    )
                    (List.filter (\reference -> Reference.getUuid reference == state.uuid) (KnowledgeModel.getQuestionReferences uuid km))
                ++ List.map (leafNode config state (Icon.regular "fa-square-check") .label)
                    (List.filter (\choice -> choice.uuid == state.uuid) (KnowledgeModel.getQuestionChoices uuid km))
        }


answerNode : Config msg -> EditorContext -> State -> Bool -> { a | uuid : String, label : String } -> Tree.Node msg
answerNode config editorContext state inside answer =
    let
        within =
            inside || answer.uuid == state.uuid
    in
    node config
        state
        { uuid = answer.uuid
        , label = answer.label
        , icon = Icon.regular "fa-circle-dot"
        , allowed = not within && answer.uuid /= state.parentUuid && state.entity == MovingQuestion
        , children =
            List.map (questionNode config editorContext state within)
                (shown Question.getUuid editorContext (KnowledgeModel.getAnswerFollowupQuestions answer.uuid editorContext.kmEditor.knowledgeModel))
        }


leafNode : Config msg -> State -> Icon -> (a -> String) -> a -> Tree.Node msg
leafNode config state icon toLabel entity =
    node config state { uuid = state.uuid, label = toLabel entity, icon = icon, allowed = False, children = [] }


node :
    Config msg
    -> State
    -> { uuid : String, label : String, icon : Icon, allowed : Bool, children : List (Tree.Node msg) }
    -> Tree.Node msg
node config state entry =
    let
        named =
            if String.isEmpty entry.label then
                Tree.withPlaceholderLabel

            else
                identity

        moving =
            entry.uuid == state.uuid

        end =
            if moving then
                [ Badge.toHtml (Badge.info (gettext "Moving" config.locale)) ]

            else
                []

        highlighted =
            if moving then
                Tree.withHighlight

            else
                identity

        branch =
            if List.isEmpty entry.children then
                identity

            else
                Tree.withChildren
                    { expanded = Set.member entry.uuid state.openNodes
                    , onToggle = config.onToggle entry.uuid (not (Set.member entry.uuid state.openNodes))
                    }
                    entry.children
    in
    Tree.node
        { key = entry.uuid
        , label =
            if String.isEmpty entry.label then
                gettext "Untitled" config.locale

            else
                entry.label
        , onSelect = config.onSelect entry.uuid
        }
        |> Tree.withIcon entry.icon
        |> Tree.withCurrent (state.selected == Just entry.uuid)
        |> Tree.withDisabled (not entry.allowed)
        |> Tree.withEnd end
        |> highlighted
        |> Tree.withTestId "km-editor_move-modal_item"
        |> named
        |> branch



-- ODDS AND ENDS


shown : (a -> String) -> EditorContext -> List a -> List a
shown toUuid editorContext =
    List.filterNot (\entity -> EditorContext.isDeleted (toUuid entity) editorContext)


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback value =
    case value of
        Just present ->
            Just present

        Nothing ->
            fallback
