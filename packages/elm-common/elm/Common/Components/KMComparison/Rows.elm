module Common.Components.KMComparison.Rows exposing
    ( Config
    , EntityDiff(..)
    , Row
    , State(..)
    , entityUuid
    , fold
    )

{-| Two knowledge models folded into one flat list of rows.

The list is what the view renders and what the tests assert, so the shape of the
comparison is decided here rather than in the markup.

**It is flat.** Nesting is a `level` on the row, not a nested type — the tree can
then be rendered with one `List.map`, one `Html.Lazy` per row, and read by a test
without walking anything.

**A section title is a row like any other** ("Questions", "Answers", …), with
`state = Section`. A section with nothing in it is not emitted, which is the only
empty state there is.

**A subtree that exists on one side only is not diffed** — its children are read
from that side and wrapped in the same constructor, so a whole added chapter is
added all the way down. Only `Changed` and `NoChange` compare their children
against each other. `childrenOf` is where that rule lives, once.

**Collapsing happens here, not in the view**: a collapsed parent's children are
dropped at fold time, and its section rows with them. `hasChildren` is computed
before that, so a collapsed parent keeps its caret.

-}

import Common.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Common.Api.Models.KnowledgeModel.Answer as Answer exposing (Answer)
import Common.Api.Models.KnowledgeModel.Chapter as Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Choice as Choice exposing (Choice)
import Common.Api.Models.KnowledgeModel.Expert as Expert exposing (Expert)
import Common.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Common.Api.Models.KnowledgeModel.Metric as Metric exposing (Metric)
import Common.Api.Models.KnowledgeModel.Phase as Phase exposing (Phase)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Common.Api.Models.KnowledgeModel.ResourceCollection as ResourceCollection exposing (ResourceCollection)
import Common.Api.Models.KnowledgeModel.ResourcePage as ResourcePage exposing (ResourcePage)
import Common.Api.Models.KnowledgeModel.Tag as Tag exposing (Tag)
import Common.Components.KMComparison.Differ as Differ exposing (DiffResult)
import Gettext exposing (gettext)
import Set exposing (Set)


{-| What to fold. `collapsed` holds the uuids whose children are hidden.
-}
type alias Config =
    { locale : Gettext.Locale
    , leftKm : KnowledgeModel
    , rightKm : KnowledgeModel
    , collapsed : Set String
    }


type alias Row =
    { id : String
    , level : Int
    , state : State
    , left : Maybe String
    , right : Maybe String
    , entity : Maybe EntityDiff
    , hasChildren : Bool
    }


type State
    = Added
    | Removed
    | Changed
    | Unchanged
    | Section


{-| What the panel opens: the entity, and what happened to it. Both sides are
carried, because the panel shows them side by side.
-}
type EntityDiff
    = ChapterDiff (DiffResult Chapter)
    | QuestionDiff (DiffResult Question)
    | AnswerDiff (DiffResult Answer)
    | ChoiceDiff (DiffResult Choice)
    | ReferenceDiff (DiffResult Reference)
    | ExpertDiff (DiffResult Expert)
    | MetricDiff (DiffResult Metric)
    | PhaseDiff (DiffResult Phase)
    | TagDiff (DiffResult Tag)
    | IntegrationDiff (DiffResult Integration)
    | ResourceCollectionDiff (DiffResult ResourceCollection)
    | ResourcePageDiff (DiffResult ResourcePage)


{-| The uuid of the entity a row opens.

It is the row's own `id` for every entity row, and the panel needs it again —
to key what it draws, and to offer the uuid for copying.

-}
entityUuid : EntityDiff -> String
entityUuid entity =
    case entity of
        ChapterDiff diff ->
            diffId .uuid diff

        QuestionDiff diff ->
            diffId Question.getUuid diff

        AnswerDiff diff ->
            diffId .uuid diff

        ChoiceDiff diff ->
            diffId .uuid diff

        ReferenceDiff diff ->
            diffId Reference.getUuid diff

        ExpertDiff diff ->
            diffId .uuid diff

        MetricDiff diff ->
            diffId .uuid diff

        PhaseDiff diff ->
            diffId .uuid diff

        TagDiff diff ->
            diffId .uuid diff

        IntegrationDiff diff ->
            diffId Integration.getUuid diff

        ResourceCollectionDiff diff ->
            diffId .uuid diff

        ResourcePageDiff diff ->
            diffId .uuid diff



-- THE FOLD


{-| The internal context. It is the config plus the two things that would
otherwise be recomputed for every reference row: the questions and resource
pages a cross-reference and a resource-page reference name themselves after.
-}
type alias Ctx =
    { locale : Gettext.Locale
    , leftKm : KnowledgeModel
    , rightKm : KnowledgeModel
    , collapsed : Set String
    , leftReferenceName : Reference -> String
    , rightReferenceName : Reference -> String
    }


fold : Config -> List Row
fold config =
    let
        ctx =
            { locale = config.locale
            , leftKm = config.leftKm
            , rightKm = config.rightKm
            , collapsed = config.collapsed
            , leftReferenceName =
                Reference.getVisibleName
                    (KnowledgeModel.getAllQuestions config.leftKm)
                    (KnowledgeModel.getAllResourcePages config.leftKm)
            , rightReferenceName =
                Reference.getVisibleName
                    (KnowledgeModel.getAllQuestions config.rightKm)
                    (KnowledgeModel.getAllResourcePages config.rightKm)
            }
    in
    List.concat
        [ topSection
            "chapters"
            (gettext "Chapters" ctx.locale)
            (Differ.createDiff .uuid Chapter.equalContent (KnowledgeModel.getChapters ctx.leftKm) (KnowledgeModel.getChapters ctx.rightKm))
            (foldChapter ctx 0)
        , topSection
            "metrics"
            (gettext "Metrics" ctx.locale)
            (Differ.createDiff .uuid Metric.equalContent (KnowledgeModel.getMetrics ctx.leftKm) (KnowledgeModel.getMetrics ctx.rightKm))
            (foldMetric 0)
        , topSection
            "phases"
            (gettext "Phases" ctx.locale)
            (Differ.createDiff .uuid Phase.equalContent (KnowledgeModel.getPhases ctx.leftKm) (KnowledgeModel.getPhases ctx.rightKm))
            (foldPhase 0)
        , topSection
            "tags"
            (gettext "Tags" ctx.locale)
            (Differ.createDiff .uuid Tag.equalContent (KnowledgeModel.getTags ctx.leftKm) (KnowledgeModel.getTags ctx.rightKm))
            (foldTag 0)
        , topSection
            "integrations"
            (gettext "Integrations" ctx.locale)
            (Differ.createDiff Integration.getUuid Integration.equalContent (KnowledgeModel.getIntegrations ctx.leftKm) (KnowledgeModel.getIntegrations ctx.rightKm))
            (foldIntegration 0)
        , topSection
            "resourceCollections"
            (gettext "Resource Collections" ctx.locale)
            (Differ.createDiff .uuid ResourceCollection.equalContent (KnowledgeModel.getResourceCollections ctx.leftKm) (KnowledgeModel.getResourceCollections ctx.rightKm))
            (foldResourceCollection ctx 0)
        ]


{-| A top-level section: its title row draws in both columns, because there is no
parent whose state would decide otherwise.
-}
topSection : String -> String -> List (DiffResult a) -> (DiffResult a -> List Row) -> List Row
topSection key title diffs foldOne =
    sectionRow 0 key title Changed diffs ++ List.concatMap foldOne diffs


{-| A parent's children: the section title and the rows under it, with the
collapsed parent's children dropped.
-}
branch : Ctx -> Int -> String -> State -> String -> String -> List (DiffResult a) -> (DiffResult a -> List Row) -> List Row
branch ctx level parentId parentState key title children foldOne =
    let
        shown =
            if Set.member parentId ctx.collapsed then
                []

            else
                children
    in
    sectionRow level (parentId ++ ":" ++ key) title parentState shown
        ++ List.concatMap foldOne shown



-- ENTITIES


foldChapter : Ctx -> Int -> DiffResult Chapter -> List Row
foldChapter ctx level chapterDiff =
    let
        uuid =
            diffId .uuid chapterDiff

        questions =
            childrenOf ctx .uuid KnowledgeModel.getChapterQuestions Question.getUuid Question.equalContent chapterDiff
    in
    entityRow
        { level = level
        , diff = chapterDiff
        , id = .uuid
        , leftTitle = .title
        , rightTitle = .title
        , entity = ChapterDiff
        , hasChildren = not (List.isEmpty questions)
        }
        :: branch ctx (level + 1) uuid (stateOf chapterDiff) "questions" (gettext "Questions" ctx.locale) questions (foldQuestion ctx (level + 1))


foldQuestion : Ctx -> Int -> DiffResult Question -> List Row
foldQuestion ctx level questionDiff =
    let
        uuid =
            diffId Question.getUuid questionDiff

        state =
            stateOf questionDiff

        childLevel =
            level + 1

        answers =
            childrenOf ctx Question.getUuid KnowledgeModel.getQuestionAnswers .uuid Answer.equalContent questionDiff

        itemQuestions =
            childrenOf ctx Question.getUuid KnowledgeModel.getQuestionItemTemplateQuestions Question.getUuid Question.equalContent questionDiff

        choices =
            childrenOf ctx Question.getUuid KnowledgeModel.getQuestionChoices .uuid Choice.equalContent questionDiff

        references =
            childrenOf ctx Question.getUuid KnowledgeModel.getQuestionReferences Reference.getUuid Reference.equalContent questionDiff

        experts =
            childrenOf ctx Question.getUuid KnowledgeModel.getQuestionExperts .uuid Expert.equalContent questionDiff
    in
    entityRow
        { level = level
        , diff = questionDiff
        , id = Question.getUuid
        , leftTitle = Question.getTitle
        , rightTitle = Question.getTitle
        , entity = QuestionDiff
        , hasChildren =
            not (List.isEmpty answers)
                || not (List.isEmpty itemQuestions)
                || not (List.isEmpty choices)
                || not (List.isEmpty references)
                || not (List.isEmpty experts)
        }
        :: List.concat
            [ branch ctx childLevel uuid state "answers" (gettext "Answers" ctx.locale) answers (foldAnswer ctx childLevel)
            , branch ctx childLevel uuid state "itemQuestions" (gettext "Item Template Questions" ctx.locale) itemQuestions (foldQuestion ctx childLevel)
            , branch ctx childLevel uuid state "choices" (gettext "Choices" ctx.locale) choices (foldChoice childLevel)
            , branch ctx childLevel uuid state "references" (gettext "References" ctx.locale) references (foldReference ctx childLevel)
            , branch ctx childLevel uuid state "experts" (gettext "Experts" ctx.locale) experts (foldExpert childLevel)
            ]


foldAnswer : Ctx -> Int -> DiffResult Answer -> List Row
foldAnswer ctx level answerDiff =
    let
        uuid =
            diffId .uuid answerDiff

        followUpQuestions =
            childrenOf ctx .uuid KnowledgeModel.getAnswerFollowupQuestions Question.getUuid Question.equalContent answerDiff
    in
    entityRow
        { level = level
        , diff = answerDiff
        , id = .uuid
        , leftTitle = .label
        , rightTitle = .label
        , entity = AnswerDiff
        , hasChildren = not (List.isEmpty followUpQuestions)
        }
        :: branch ctx (level + 1) uuid (stateOf answerDiff) "followUpQuestions" (gettext "Follow-up Questions" ctx.locale) followUpQuestions (foldQuestion ctx (level + 1))


foldResourceCollection : Ctx -> Int -> DiffResult ResourceCollection -> List Row
foldResourceCollection ctx level resourceCollectionDiff =
    let
        uuid =
            diffId .uuid resourceCollectionDiff

        resourcePages =
            childrenOf ctx .uuid KnowledgeModel.getResourceCollectionResourcePages .uuid ResourcePage.equalContent resourceCollectionDiff
    in
    entityRow
        { level = level
        , diff = resourceCollectionDiff
        , id = .uuid
        , leftTitle = .title
        , rightTitle = .title
        , entity = ResourceCollectionDiff
        , hasChildren = not (List.isEmpty resourcePages)
        }
        :: branch ctx (level + 1) uuid (stateOf resourceCollectionDiff) "resourcePages" (gettext "Resource Pages" ctx.locale) resourcePages (foldResourcePage (level + 1))



-- LEAVES


foldChoice : Int -> DiffResult Choice -> List Row
foldChoice level choiceDiff =
    [ leafRow level choiceDiff .uuid .label .label ChoiceDiff ]


foldReference : Ctx -> Int -> DiffResult Reference -> List Row
foldReference ctx level referenceDiff =
    [ entityRow
        { level = level
        , diff = referenceDiff
        , id = Reference.getUuid
        , leftTitle = ctx.leftReferenceName
        , rightTitle = ctx.rightReferenceName
        , entity = ReferenceDiff
        , hasChildren = False
        }
    ]


foldExpert : Int -> DiffResult Expert -> List Row
foldExpert level expertDiff =
    [ leafRow level expertDiff .uuid Expert.getVisibleName Expert.getVisibleName ExpertDiff ]


foldMetric : Int -> DiffResult Metric -> List Row
foldMetric level metricDiff =
    [ leafRow level metricDiff .uuid .title .title MetricDiff ]


foldPhase : Int -> DiffResult Phase -> List Row
foldPhase level phaseDiff =
    [ leafRow level phaseDiff .uuid .title .title PhaseDiff ]


foldTag : Int -> DiffResult Tag -> List Row
foldTag level tagDiff =
    [ leafRow level tagDiff .uuid .name .name TagDiff ]


foldIntegration : Int -> DiffResult Integration -> List Row
foldIntegration level integrationDiff =
    [ leafRow level integrationDiff Integration.getUuid Integration.getName Integration.getName IntegrationDiff ]


foldResourcePage : Int -> DiffResult ResourcePage -> List Row
foldResourcePage level resourcePageDiff =
    [ leafRow level resourcePageDiff .uuid .title .title ResourcePageDiff ]



-- ROWS


leafRow : Int -> DiffResult a -> (a -> String) -> (a -> String) -> (a -> String) -> (DiffResult a -> EntityDiff) -> Row
leafRow level diff id leftTitle rightTitle entity =
    entityRow
        { level = level
        , diff = diff
        , id = id
        , leftTitle = leftTitle
        , rightTitle = rightTitle
        , entity = entity
        , hasChildren = False
        }


entityRow :
    { level : Int
    , diff : DiffResult a
    , id : a -> String
    , leftTitle : a -> String
    , rightTitle : a -> String
    , entity : DiffResult a -> EntityDiff
    , hasChildren : Bool
    }
    -> Row
entityRow props =
    let
        ( left, right ) =
            case props.diff of
                Differ.Added item ->
                    ( Nothing, Just (props.rightTitle item) )

                Differ.Removed item ->
                    ( Just (props.leftTitle item), Nothing )

                Differ.Changed leftItem rightItem ->
                    ( Just (props.leftTitle leftItem), Just (props.rightTitle rightItem) )

                Differ.NoChange leftItem rightItem ->
                    ( Just (props.leftTitle leftItem), Just (props.rightTitle rightItem) )
    in
    { id = diffId props.id props.diff
    , level = props.level
    , state = stateOf props.diff
    , left = left
    , right = right
    , entity = Just (props.entity props.diff)
    , hasChildren = props.hasChildren
    }


{-| A section title. It draws in the columns its parent draws in, so an added
chapter's "Questions" appears only on the right.
-}
sectionRow : Int -> String -> String -> State -> List a -> List Row
sectionRow level id title parentState items =
    if List.isEmpty items then
        []

    else
        let
            ( hasLeft, hasRight ) =
                case parentState of
                    Added ->
                        ( False, True )

                    Removed ->
                        ( True, False )

                    _ ->
                        ( True, True )

            column shows =
                if shows then
                    Just title

                else
                    Nothing
        in
        [ { id = id
          , level = level
          , state = Section
          , left = column hasLeft
          , right = column hasRight
          , entity = Nothing
          , hasChildren = False
          }
        ]



-- THE RULE OF §2.3


{-| The children of a parent that may exist on one side only.

A parent that was added or removed is not compared: its children are read from
the side it is on and wrapped in the same constructor, so the whole subtree
carries the parent's state. Only a parent on both sides has its children diffed.

-}
childrenOf :
    Ctx
    -> (parent -> String)
    -> (String -> KnowledgeModel -> List child)
    -> (child -> id)
    -> (child -> child -> Bool)
    -> DiffResult parent
    -> List (DiffResult child)
childrenOf ctx parentUuid get toId equalContent parentDiff =
    let
        diffBoth leftParent rightParent =
            Differ.createDiff toId
                equalContent
                (get (parentUuid leftParent) ctx.leftKm)
                (get (parentUuid rightParent) ctx.rightKm)
    in
    case parentDiff of
        Differ.Added parent ->
            List.map Differ.Added (get (parentUuid parent) ctx.rightKm)

        Differ.Removed parent ->
            List.map Differ.Removed (get (parentUuid parent) ctx.leftKm)

        Differ.Changed leftParent rightParent ->
            diffBoth leftParent rightParent

        Differ.NoChange leftParent rightParent ->
            diffBoth leftParent rightParent


stateOf : DiffResult a -> State
stateOf diff =
    case diff of
        Differ.Added _ ->
            Added

        Differ.Removed _ ->
            Removed

        Differ.Changed _ _ ->
            Changed

        Differ.NoChange _ _ ->
            Unchanged


diffId : (a -> String) -> DiffResult a -> String
diffId toId diff =
    case diff of
        Differ.Added item ->
            toId item

        Differ.Removed item ->
            toId item

        Differ.Changed leftItem _ ->
            toId leftItem

        Differ.NoChange leftItem _ ->
            toId leftItem
