module Common.Pages.KMEditor.Editor.Tree exposing (AddTo(..), Config, view)

{-| The knowledge model as a tree of places to go.

`Ui.Tree` does the shape — nested lists, a disclosure button per branch,
`aria-current` on the place you are in — and this decides what the nodes are.

Two things it adds to what the questionnaire needed:

  - **What has been done to an entity is said in a word**, not only in a
    color. The wizard marks added, edited and deleted with green, blue and red
    alone, which fails 1.4.1; here each keeps its color and gains a badge.
  - **"Add" rows follow the open entity.** A collection shows its "Add" row
    only while one of the things in it is open, as the wizard does: a row at
    the end of every collection at once is a column of them down the tree, and
    the tree is for finding things. A collection with nothing in it is filled
    from its list in the form pane, which carries the same button.
  - **The knowledge model itself does not fold.** It is the one thing every
    entry is inside, so a caret that hides the whole tree buys nothing and
    costs every row a level of indent. Its name is the first entry and what is
    in it follows at the top level, a level shallower than it would otherwise
    be.
  - **A deleted entity is out of reach.** It stays in the tree, red and marked
    "Deleted", but there is nothing behind it to open.

An entity added and deleted again before publishing is not shown at all; the
rest is ordered with the deleted ones last.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Integration as Integration
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Common.Api.Models.KnowledgeModel.Reference as Reference
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Ui.Badge as Badge
import Ui.Icon as Icon exposing (Icon)
import Ui.Tree as Tree
import Uuid


type alias Config msg =
    { locale : Gettext.Locale
    , onSelect : String -> msg
    , onToggle : String -> Bool -> msg
    , onAdd : AddTo -> String -> msg
    }


{-| What an "Add" row makes, and under which parent.
-}
type AddTo
    = AddChapter
    | AddQuestion
    | AddAnswer
    | AddChoice
    | AddReference
    | AddExpert
    | AddMetric
    | AddPhase
    | AddTag
    | AddIntegration
    | AddResourceCollection
    | AddResourcePage


view : Config msg -> EditorContext -> Html msg
view config editorContext =
    Tree.toHtml
        { label = gettext "Knowledge model" config.locale
        , words =
            { expand = \label -> gettext "Show what is in " config.locale ++ label
            , collapse = \label -> gettext "Hide what is in " config.locale ++ label
            }
        }
        (knowledgeModelNode config editorContext :: knowledgeModelChildren config editorContext)



-- NODES


knowledgeModelNode : Config msg -> EditorContext -> Tree.Node msg
knowledgeModelNode config editorContext =
    entityNode config
        editorContext
        (Uuid.toString editorContext.kmEditor.knowledgeModel.uuid)
        editorContext.kmEditor.name
        (Icon.solid "fa-sitemap")
        |> Tree.withRoot


{-| What is in the knowledge model, at the top level: see the note above about
why it does not fold.
-}
knowledgeModelChildren : Config msg -> EditorContext -> List (Tree.Node msg)
knowledgeModelChildren config editorContext =
    let
        km =
            editorContext.kmEditor.knowledgeModel

        uuid =
            Uuid.toString km.uuid

        collection toUuid icon toLabel entities addTo =
            withAdd config
                editorContext
                addTo
                uuid
                toUuid
                (shown toUuid editorContext entities)
                (List.map (leafNode config editorContext icon toUuid toLabel))

        chapters =
            shown .uuid editorContext (KnowledgeModel.getChapters km)

        resourceCollections =
            shown .uuid editorContext (KnowledgeModel.getResourceCollections km)
    in
    List.concat
        [ withAdd config editorContext AddChapter uuid .uuid chapters (List.map (chapterNode config editorContext))
        , collection .uuid (Icon.solid "fa-chart-column") .title (KnowledgeModel.getMetrics km) AddMetric
        , collection .uuid (Icon.regular "fa-clock") .title (KnowledgeModel.getPhases km) AddPhase
        , collection .uuid (Icon.solid "fa-tag") .name (KnowledgeModel.getTags km) AddTag
        , collection Integration.getUuid (Icon.solid "fa-right-left") Integration.getVisibleName (KnowledgeModel.getIntegrations km) AddIntegration
        , withAdd config editorContext AddResourceCollection uuid .uuid resourceCollections (List.map (resourceCollectionNode config editorContext))
        ]


chapterNode : Config msg -> EditorContext -> { a | uuid : String, title : String } -> Tree.Node msg
chapterNode config editorContext chapter =
    let
        questions =
            shown Question.getUuid editorContext (KnowledgeModel.getChapterQuestions chapter.uuid editorContext.kmEditor.knowledgeModel)
    in
    entityNode config editorContext chapter.uuid chapter.title (Icon.regular "fa-file")
        |> branch config
            editorContext
            chapter.uuid
            (withAdd config editorContext AddQuestion chapter.uuid Question.getUuid questions (List.map (questionNode config editorContext)))


questionNode : Config msg -> EditorContext -> Question -> Tree.Node msg
questionNode config editorContext question =
    let
        uuid =
            Question.getUuid question

        km =
            editorContext.kmEditor.knowledgeModel

        typeChildren =
            case question of
                OptionsQuestion _ _ ->
                    withAdd config
                        editorContext
                        AddAnswer
                        uuid
                        .uuid
                        (shown .uuid editorContext (KnowledgeModel.getQuestionAnswers uuid km))
                        (List.map (answerNode config editorContext))

                ListQuestion _ _ ->
                    withAdd config
                        editorContext
                        AddQuestion
                        uuid
                        Question.getUuid
                        (shown Question.getUuid editorContext (KnowledgeModel.getQuestionItemTemplateQuestions uuid km))
                        (List.map (questionNode config editorContext))

                MultiChoiceQuestion _ _ ->
                    withAdd config
                        editorContext
                        AddChoice
                        uuid
                        .uuid
                        (shown .uuid editorContext (KnowledgeModel.getQuestionChoices uuid km))
                        (List.map (leafNode config editorContext (Icon.regular "fa-square-check") .uuid .label))

                _ ->
                    []

        references =
            withAdd config
                editorContext
                AddReference
                uuid
                Reference.getUuid
                (shown Reference.getUuid editorContext (KnowledgeModel.getQuestionReferences uuid km))
                (List.map (referenceNode config editorContext))

        experts =
            withAdd config
                editorContext
                AddExpert
                uuid
                .uuid
                (shown .uuid editorContext (KnowledgeModel.getQuestionExperts uuid km))
                (List.map (leafNode config editorContext (Icon.regular "fa-user") .uuid .name))
    in
    entityNode config editorContext uuid (Question.getTitle question) (Icon.regular "fa-comment")
        |> branch config editorContext uuid (typeChildren ++ references ++ experts)


answerNode : Config msg -> EditorContext -> { a | uuid : String, label : String } -> Tree.Node msg
answerNode config editorContext answer =
    let
        followUps =
            shown Question.getUuid editorContext (KnowledgeModel.getAnswerFollowupQuestions answer.uuid editorContext.kmEditor.knowledgeModel)
    in
    entityNode config editorContext answer.uuid answer.label (Icon.regular "fa-circle-dot")
        |> branch config
            editorContext
            answer.uuid
            (withAdd config editorContext AddQuestion answer.uuid Question.getUuid followUps (List.map (questionNode config editorContext)))


resourceCollectionNode : Config msg -> EditorContext -> { a | uuid : String, title : String } -> Tree.Node msg
resourceCollectionNode config editorContext collection =
    let
        pages =
            shown .uuid editorContext (KnowledgeModel.getResourceCollectionResourcePages collection.uuid editorContext.kmEditor.knowledgeModel)
    in
    entityNode config editorContext collection.uuid collection.title (Icon.solid "fa-book")
        |> branch config
            editorContext
            collection.uuid
            (withAdd config
                editorContext
                AddResourcePage
                collection.uuid
                .uuid
                pages
                (List.map (leafNode config editorContext (Icon.regular "fa-file-lines") .uuid .title))
            )


referenceNode : Config msg -> EditorContext -> Reference.Reference -> Tree.Node msg
referenceNode config editorContext value =
    let
        km =
            editorContext.kmEditor.knowledgeModel
    in
    entityNode config
        editorContext
        (Reference.getUuid value)
        (Reference.getVisibleName (KnowledgeModel.getAllQuestions km) (KnowledgeModel.getAllResourcePages km) value)
        (Icon.regular "fa-bookmark")


leafNode : Config msg -> EditorContext -> Icon -> (a -> String) -> (a -> String) -> a -> Tree.Node msg
leafNode config editorContext icon toUuid toLabel entity =
    entityNode config editorContext (toUuid entity) (toLabel entity) icon



-- WHAT A NODE IS


{-| An entity: where it goes, what it is called, and what has been done to it.
-}
entityNode : Config msg -> EditorContext -> String -> String -> Icon -> Tree.Node msg
entityNode config editorContext uuid label icon =
    let
        named =
            if String.isEmpty label then
                Tree.withPlaceholderLabel

            else
                identity

        toned =
            case stateTone editorContext uuid of
                Just tone ->
                    Tree.withTone tone

                Nothing ->
                    identity
    in
    Tree.node
        { key = uuid
        , label =
            if String.isEmpty label then
                gettext "Untitled" config.locale

            else
                label
        , onSelect = config.onSelect uuid
        }
        |> Tree.withIcon icon
        |> Tree.withCurrent (EditorContext.isActive uuid editorContext)
        |> Tree.withEnd (stateBadge config editorContext uuid)
        |> Tree.withDisabled (EditorContext.isDeleted uuid editorContext)
        |> Tree.withTestId "km-editor_tree_entity"
        |> named
        |> toned


{-| The same three states as a color on the label. The badge is what says
which; this is what makes it visible down a long tree.
-}
stateTone : EditorContext -> String -> Maybe Tree.Tone
stateTone editorContext uuid =
    if EditorContext.isDeleted uuid editorContext then
        Just Tree.Danger

    else if EditorContext.isAdded uuid editorContext then
        Just Tree.Success

    else if EditorContext.isEdited uuid editorContext then
        Just Tree.Info

    else
        Nothing


{-| The state in a word. It is inside the entry, so it is part of the entry's
name: a screen reader says "Chapter 2, added".
-}
stateBadge : Config msg -> EditorContext -> String -> List (Html msg)
stateBadge config editorContext uuid =
    if EditorContext.isDeleted uuid editorContext then
        [ Badge.toHtml (Badge.danger (gettext "Deleted" config.locale)) ]

    else if EditorContext.isAdded uuid editorContext then
        [ Badge.toHtml (Badge.success (gettext "Added" config.locale)) ]

    else if EditorContext.isEdited uuid editorContext then
        [ Badge.toHtml (Badge.info (gettext "Edited" config.locale)) ]

    else
        []


{-| A collection, with the row that adds to it after it — but only while one of
the things in it is open, which is when adding a sibling is what somebody is
likely to want. See the note at the top.
-}
withAdd :
    Config msg
    -> EditorContext
    -> AddTo
    -> String
    -> (a -> String)
    -> List a
    -> (List a -> List (Tree.Node msg))
    -> List (Tree.Node msg)
withAdd config editorContext addTo parentUuid toUuid entities build =
    build entities
        ++ (if List.any (\entity -> EditorContext.isActive (toUuid entity) editorContext) entities then
                [ addNode config addTo parentUuid ]

            else
                []
           )


{-| A row that makes a new entity.
-}
addNode : Config msg -> AddTo -> String -> Tree.Node msg
addNode config addTo parentUuid =
    Tree.node
        { key = parentUuid ++ ":add:" ++ addLabel config addTo
        , label = addLabel config addTo
        , onSelect = config.onAdd addTo parentUuid
        }
        |> Tree.withIcon (Icon.solid "fa-plus")
        |> Tree.withAction
        |> Tree.withTestId "km-editor_tree_add"


addLabel : Config msg -> AddTo -> String
addLabel config addTo =
    case addTo of
        AddChapter ->
            gettext "Add chapter" config.locale

        AddQuestion ->
            gettext "Add question" config.locale

        AddAnswer ->
            gettext "Add answer" config.locale

        AddChoice ->
            gettext "Add choice" config.locale

        AddReference ->
            gettext "Add reference" config.locale

        AddExpert ->
            gettext "Add expert" config.locale

        AddMetric ->
            gettext "Add metric" config.locale

        AddPhase ->
            gettext "Add phase" config.locale

        AddTag ->
            gettext "Add tag" config.locale

        AddIntegration ->
            gettext "Add integration" config.locale

        AddResourceCollection ->
            gettext "Add resource collection" config.locale

        AddResourcePage ->
            gettext "Add resource page" config.locale


{-| A deleted entity has nothing under it worth opening, so it draws no branch.
Neither does one with nothing under it: a caret that opens onto nothing is a
promise the tree cannot keep, and a reader looking for where something is kept
follows every one of them.
-}
branch : Config msg -> EditorContext -> String -> List (Tree.Node msg) -> Tree.Node msg -> Tree.Node msg
branch config editorContext uuid children =
    if EditorContext.isDeleted uuid editorContext || List.isEmpty children then
        identity

    else
        Tree.withChildren
            { expanded = EditorContext.isNodeOpen uuid editorContext
            , onToggle = config.onToggle uuid (not (EditorContext.isNodeOpen uuid editorContext))
            }
            children


{-| What the tree shows of a collection: not what was added and deleted again,
and what is deleted last.
-}
shown : (a -> String) -> EditorContext -> List a -> List a
shown toUuid editorContext entities =
    entities
        |> List.filterNot (\entity -> EditorContext.isAddedAndDeleted (toUuid entity) editorContext)
        |> EditorContext.sortDeleted toUuid editorContext
