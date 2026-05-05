module Wizard.Components.KMComparison exposing
    ( Model
    , Msg
    , UpdateConfig
    , compare
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError exposing (ApiError)
import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faKmAnswer, faKmChapter, faKmChoice, faKmIntegration, faKmMetric, faKmPhase, faKmQuestion, faKmReference, faKmResourceCollection, faKmTag, fas)
import Common.Components.Page as Page
import Common.Utils.RequestHelpers as RequestHelpers
import Flip exposing (flip)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, span, strong, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Uuid exposing (Uuid)
import Version
import Wizard.Api.KnowledgeModels as KnowledgeModels
import Wizard.Api.Models.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Wizard.Api.Models.KnowledgeModel.Answer as Answer exposing (Answer)
import Wizard.Api.Models.KnowledgeModel.Chapter as Chapter exposing (Chapter)
import Wizard.Api.Models.KnowledgeModel.Choice as Choice exposing (Choice)
import Wizard.Api.Models.KnowledgeModel.Expert as Expert exposing (Expert)
import Wizard.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Wizard.Api.Models.KnowledgeModel.Metric as Metric exposing (Metric)
import Wizard.Api.Models.KnowledgeModel.Phase as Phase exposing (Phase)
import Wizard.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Wizard.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference)
import Wizard.Api.Models.KnowledgeModel.ResourceCollection as ResourceCollection exposing (ResourceCollection)
import Wizard.Api.Models.KnowledgeModel.ResourcePage as ResourcePage exposing (ResourcePage)
import Wizard.Api.Models.KnowledgeModel.Tag as Tag exposing (Tag)
import Wizard.Api.Models.KnowledgeModelPackageDetail exposing (KnowledgeModelPackageDetail)
import Wizard.Components.ItemIcon as ItemIcon
import Wizard.Components.KMComparison.CompareInput exposing (CompareInput)
import Wizard.Components.KMComparison.Differ as Differ
import Wizard.Components.KMComparison.SidePanel as SidePanel
import Wizard.Components.Tag as Tag
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { compareInput : Maybe CompareInput
    , leftKmUuid : Uuid
    , rightKmUuid : Uuid
    , leftKm : ActionResult KnowledgeModel
    , rightKm : ActionResult KnowledgeModel
    , collapsedUuids : Set String
    , sidePanel : Maybe SidePanel.SidePanelState
    }


type Msg
    = Compare CompareInput
    | FetchLeftKmCompleted (Result ApiError KnowledgeModel)
    | FetchRightKmCompleted (Result ApiError KnowledgeModel)
    | ToggleCollapse String
    | SetSidePanel (Maybe SidePanel.SidePanelState)


compare : CompareInput -> Msg
compare =
    Compare


initialModel : Model
initialModel =
    { compareInput = Nothing
    , leftKmUuid = Uuid.nil
    , rightKmUuid = Uuid.nil
    , leftKm = ActionResult.Unset
    , rightKm = ActionResult.Unset
    , collapsedUuids = Set.empty
    , sidePanel = Nothing
    }


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Compare compareInput ->
            let
                leftKmCmd =
                    KnowledgeModels.fetchPreview appState (Just compareInput.leftVersion) [] compareInput.leftTags (cfg.wrapMsg << FetchLeftKmCompleted)

                rightKmCmd =
                    KnowledgeModels.fetchPreview appState (Just compareInput.rightVersion) [] compareInput.rightTags (cfg.wrapMsg << FetchRightKmCompleted)
            in
            ( { model
                | compareInput = Just compareInput
                , leftKmUuid = compareInput.leftVersion
                , rightKmUuid = compareInput.rightVersion
                , leftKm = ActionResult.Loading
                , rightKm = ActionResult.Loading
              }
            , Cmd.batch [ leftKmCmd, rightKmCmd ]
            )

        FetchLeftKmCompleted result ->
            RequestHelpers.applyResult
                { setResult = \km m -> { m | leftKm = km }
                , defaultError = "Unable to fetch Knowledge Model."
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        FetchRightKmCompleted result ->
            RequestHelpers.applyResult
                { setResult = \km m -> { m | rightKm = km }
                , defaultError = "Unable to fetch Knowledge Model."
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        ToggleCollapse uuid ->
            let
                newCollapsedUuids =
                    if Set.member uuid model.collapsedUuids then
                        Set.remove uuid model.collapsedUuids

                    else
                        Set.insert uuid model.collapsedUuids
            in
            ( { model | collapsedUuids = newCollapsedUuids }, Cmd.none )

        SetSidePanel maybeSidePanel ->
            ( { model | sidePanel = maybeSidePanel }, Cmd.none )


view : AppState -> Model -> Html Msg
view appState model =
    let
        kms =
            ActionResult.combine model.leftKm model.rightKm
    in
    Page.actionResultView appState (viewPage appState model) kms


viewPage : AppState -> Model -> ( KnowledgeModel, KnowledgeModel ) -> Html Msg
viewPage appState model ( leftKm, rightKm ) =
    viewComparison appState.locale model leftKm rightKm


viewComparison : Gettext.Locale -> Model -> KnowledgeModel -> KnowledgeModel -> Html Msg
viewComparison locale model leftKm rightKM =
    let
        ( packageLeftInfo, packageRightInfo ) =
            case model.compareInput of
                Just compareInput ->
                    ( viewPackageInfo
                        { package = compareInput.leftPackage
                        , versionUuid = compareInput.leftVersion
                        , selectedTags = compareInput.leftTags
                        , km = leftKm
                        }
                    , viewPackageInfo
                        { package = compareInput.rightPackage
                        , versionUuid = compareInput.rightVersion
                        , selectedTags = compareInput.rightTags
                        , km = rightKM
                        }
                    )

                Nothing ->
                    ( Html.nothing, Html.nothing )

        foldContext =
            { locale = locale
            , leftKm = leftKm
            , rightKm = rightKM
            , indentLevel = 0
            , collapsedUuids = model.collapsedUuids
            }

        sidePanelView =
            case model.sidePanel of
                Just sidePanel ->
                    let
                        sidePanelProps =
                            { locale = locale
                            , leftKm = leftKm
                            , rightKm = rightKM
                            , closeMsg = SetSidePanel Nothing
                            }
                    in
                    SidePanel.viewSidePanel sidePanelProps sidePanel

                Nothing ->
                    Html.nothing
    in
    div
        [ class "kmComparison"
        ]
        [ div [ class "kmComparison__content" ]
            [ table [ class "table table-bordered table-hover w-100" ]
                [ thead []
                    [ tr []
                        [ td [ class "align-top" ] [ packageLeftInfo ]
                        , td [ class "align-top" ] [ packageRightInfo ]
                        ]
                    ]
                , tbody [] (foldKMRows foldContext)
                ]
            ]
        , sidePanelView
        ]


type alias ViewPackageProps =
    { package : KnowledgeModelPackageDetail
    , versionUuid : Uuid
    , selectedTags : List String
    , km : KnowledgeModel
    }


viewPackageInfo : ViewPackageProps -> Html Msg
viewPackageInfo props =
    let
        selectedVersion =
            List.find (\version -> version.uuid == props.versionUuid) props.package.versions
                |> Maybe.unwrap props.package.version .version
                |> Version.toString

        selectedTags =
            List.filterMap (flip KnowledgeModel.getTag props.km) props.selectedTags

        tagsView =
            if List.isEmpty selectedTags then
                Html.nothing

            else
                div [ class "mt-2" ]
                    [ Tag.viewList { showDescription = False } selectedTags ]
    in
    div [ class "d-flex" ]
        [ div [] [ ItemIcon.view { text = props.package.name, image = Nothing } ]
        , div [ class "ps-2" ]
            [ div []
                [ strong [] [ text props.package.name ]
                , Badge.light [ class "ms-1" ] [ text selectedVersion ]
                ]
            , div [] [ text props.package.description ]
            , tagsView
            ]
        ]


type alias FoldContext =
    { locale : Gettext.Locale
    , leftKm : KnowledgeModel
    , rightKm : KnowledgeModel
    , indentLevel : Int
    , collapsedUuids : Set String
    }


addIndentLevel : FoldContext -> FoldContext
addIndentLevel ctx =
    { ctx | indentLevel = ctx.indentLevel + 1 }


foldKMRows : FoldContext -> List (Html Msg)
foldKMRows ctx =
    let
        leftChapters =
            KnowledgeModel.getChapters ctx.leftKm

        rightChapters =
            KnowledgeModel.getChapters ctx.rightKm

        chapterDiffs =
            Differ.createDiff .uuid Chapter.equalContent leftChapters rightChapters

        chapters =
            List.concatMap (foldChapter ctx) chapterDiffs

        chaptersTitle =
            viewTitleRow
                { title = gettext "Chapters" ctx.locale
                , indentLevel = ctx.indentLevel
                , items = chapterDiffs
                , item = Differ.Changed () ()
                }

        leftMetrics =
            KnowledgeModel.getMetrics ctx.leftKm

        rightMetrics =
            KnowledgeModel.getMetrics ctx.rightKm

        metricDiffs =
            Differ.createDiff .uuid Metric.equalContent leftMetrics rightMetrics

        metrics =
            List.concatMap (foldMetric ctx) metricDiffs

        metricsTitle =
            viewTitleRow
                { title = gettext "Metrics" ctx.locale
                , indentLevel = ctx.indentLevel
                , items = metricDiffs
                , item = Differ.Changed () ()
                }

        leftPhases =
            KnowledgeModel.getPhases ctx.leftKm

        rightPhases =
            KnowledgeModel.getPhases ctx.rightKm

        phaseDiffs =
            Differ.createDiff .uuid Phase.equalContent leftPhases rightPhases

        phases =
            List.concatMap (foldPhase ctx) phaseDiffs

        phasesTitle =
            viewTitleRow
                { title = gettext "Phases" ctx.locale
                , indentLevel = ctx.indentLevel
                , items = phaseDiffs
                , item = Differ.Changed () ()
                }

        leftTags =
            KnowledgeModel.getTags ctx.leftKm

        rightTags =
            KnowledgeModel.getTags ctx.rightKm

        tagDiffs =
            Differ.createDiff .uuid Tag.equalContent leftTags rightTags

        tags =
            List.concatMap (foldTag ctx) tagDiffs

        tagsTitle =
            viewTitleRow
                { title = gettext "Tags" ctx.locale
                , indentLevel = ctx.indentLevel
                , items = tagDiffs
                , item = Differ.Changed () ()
                }

        leftIntegrations =
            KnowledgeModel.getIntegrations ctx.leftKm

        rightIntegrations =
            KnowledgeModel.getIntegrations ctx.rightKm

        integrationDiffs =
            Differ.createDiff Integration.getUuid Integration.equalContent leftIntegrations rightIntegrations

        integrations =
            List.concatMap (foldIntegration ctx) integrationDiffs

        integrationsTitle =
            viewTitleRow
                { title = gettext "Integrations" ctx.locale
                , indentLevel = ctx.indentLevel
                , items = integrationDiffs
                , item = Differ.Changed () ()
                }

        leftResourceCollections =
            KnowledgeModel.getResourceCollections ctx.leftKm

        rightResourceCollections =
            KnowledgeModel.getResourceCollections ctx.rightKm

        resourceCollectionDiffs =
            Differ.createDiff .uuid ResourceCollection.equalContent leftResourceCollections rightResourceCollections

        resourceCollections =
            List.concatMap (foldResourceCollection ctx) resourceCollectionDiffs

        resourceCollectionsTitle =
            viewTitleRow
                { title = gettext "Resource Collections" ctx.locale
                , indentLevel = ctx.indentLevel
                , items = resourceCollectionDiffs
                , item = Differ.Changed () ()
                }
    in
    chaptersTitle
        ++ chapters
        ++ metricsTitle
        ++ metrics
        ++ phasesTitle
        ++ phases
        ++ tagsTitle
        ++ tags
        ++ integrationsTitle
        ++ integrations
        ++ resourceCollectionsTitle
        ++ resourceCollections


foldChapter : FoldContext -> Differ.DiffResult Chapter -> List (Html Msg)
foldChapter ctx chapterDiff =
    let
        chapterCtx =
            addIndentLevel ctx

        chapterRow questions =
            viewItemRow
                { indentLevel = ctx.indentLevel
                , item = chapterDiff
                , icon = faKmChapter
                , getTitle = GetTitleSimple .title
                , getUuid = .uuid
                , collapsedUuids = ctx.collapsedUuids
                , hasChildren = not (List.isEmpty questions)
                , sidePanelWrapper = SidePanel.SidePanelChapter
                }

        filterCollapsed chapter items =
            if Set.member chapter.uuid ctx.collapsedUuids then
                []

            else
                items

        createQuestionTitleRow questions =
            viewTitleRow
                { title = gettext "Questions" ctx.locale
                , indentLevel = chapterCtx.indentLevel
                , items = questions
                , item = chapterDiff
                }

        createQuestionRows questions =
            List.concatMap (foldQuestion chapterCtx) questions

        createSingleChapterRow mapper chapter km =
            let
                questions =
                    KnowledgeModel.getChapterQuestions chapter.uuid km
                        |> List.map mapper

                filteredQuestions =
                    filterCollapsed chapter questions
            in
            chapterRow questions
                :: createQuestionTitleRow filteredQuestions
                ++ createQuestionRows filteredQuestions

        createBothChaptersRow leftChapter rightChapter =
            let
                leftQuestions =
                    KnowledgeModel.getChapterQuestions leftChapter.uuid ctx.leftKm

                rightQuestions =
                    KnowledgeModel.getChapterQuestions rightChapter.uuid ctx.rightKm

                questions =
                    Differ.createDiff Question.getUuid Question.equalContent leftQuestions rightQuestions

                filteredQuestions =
                    filterCollapsed leftChapter questions
            in
            chapterRow questions
                :: createQuestionTitleRow filteredQuestions
                ++ createQuestionRows filteredQuestions
    in
    case chapterDiff of
        Differ.Added chapter ->
            createSingleChapterRow Differ.Added chapter ctx.rightKm

        Differ.Removed chapter ->
            createSingleChapterRow Differ.Removed chapter ctx.leftKm

        Differ.Changed leftChapter rightChapter ->
            createBothChaptersRow leftChapter rightChapter

        Differ.NoChange leftChapter rightChapter ->
            createBothChaptersRow leftChapter rightChapter


foldQuestion : FoldContext -> Differ.DiffResult Question -> List (Html Msg)
foldQuestion ctx questionDiff =
    let
        questionCtx =
            addIndentLevel ctx

        questionRow answers itemQuestions choices references experts =
            viewItemRow
                { indentLevel = ctx.indentLevel
                , item = questionDiff
                , icon = faKmQuestion
                , getTitle = GetTitleSimple Question.getTitle
                , getUuid = Question.getUuid
                , collapsedUuids = ctx.collapsedUuids
                , hasChildren =
                    not (List.isEmpty answers)
                        || not (List.isEmpty itemQuestions)
                        || not (List.isEmpty choices)
                        || not (List.isEmpty references)
                        || not (List.isEmpty experts)
                , sidePanelWrapper = SidePanel.SidePanelQuestion
                }

        filterCollapsed question items =
            if Set.member (Question.getUuid question) ctx.collapsedUuids then
                []

            else
                items

        createAnswerTitleRow answers =
            viewTitleRow
                { title = gettext "Answers" ctx.locale
                , indentLevel = questionCtx.indentLevel
                , items = answers
                , item = questionDiff
                }

        createAnswerRows answers =
            List.concatMap (foldAnswer questionCtx) answers

        createItemQuestionTitleRow itemQuestions =
            viewTitleRow
                { title = gettext "Item Template Questions" ctx.locale
                , indentLevel = questionCtx.indentLevel
                , items = itemQuestions
                , item = questionDiff
                }

        createItemQuestionRows questions =
            List.concatMap (foldQuestion questionCtx) questions

        createChoiceTitleRow choices =
            viewTitleRow
                { title = gettext "Choices" ctx.locale
                , indentLevel = questionCtx.indentLevel
                , items = choices
                , item = questionDiff
                }

        createChoiceRows choices =
            List.concatMap (foldChoice questionCtx) choices

        createReferenceTitleRow references =
            viewTitleRow
                { title = gettext "References" ctx.locale
                , indentLevel = questionCtx.indentLevel
                , items = references
                , item = questionDiff
                }

        createReferenceRows references =
            List.concatMap (foldReference questionCtx) references

        createExpertTitleRow experts =
            viewTitleRow
                { title = gettext "Experts" ctx.locale
                , indentLevel = questionCtx.indentLevel
                , items = experts
                , item = questionDiff
                }

        createExpertRows experts =
            List.concatMap (foldExpert questionCtx) experts

        createSingleQuestionRow answerMapper itemQuestionsMapper choiceMapper referenceMapper expertMapper question km =
            let
                answers =
                    KnowledgeModel.getQuestionAnswers (Question.getUuid question) km
                        |> List.map answerMapper

                filteredAnswers =
                    filterCollapsed question answers

                itemQuestions =
                    KnowledgeModel.getQuestionItemTemplateQuestions (Question.getUuid question) km
                        |> List.map itemQuestionsMapper

                filteredItemQuestions =
                    filterCollapsed question itemQuestions

                choices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid question) km
                        |> List.map choiceMapper

                filteredChoices =
                    filterCollapsed question choices

                references =
                    KnowledgeModel.getQuestionReferences (Question.getUuid question) km
                        |> List.map referenceMapper

                filteredReferences =
                    filterCollapsed question references

                experts =
                    KnowledgeModel.getQuestionExperts (Question.getUuid question) km
                        |> List.map expertMapper

                filteredExperts =
                    filterCollapsed question experts
            in
            questionRow answers itemQuestions choices references experts
                :: createAnswerTitleRow filteredAnswers
                ++ createAnswerRows filteredAnswers
                ++ createItemQuestionTitleRow filteredItemQuestions
                ++ createItemQuestionRows filteredItemQuestions
                ++ createChoiceTitleRow filteredChoices
                ++ createChoiceRows filteredChoices
                ++ createReferenceTitleRow filteredReferences
                ++ createReferenceRows filteredReferences
                ++ createExpertTitleRow filteredExperts
                ++ createExpertRows filteredExperts

        createBothQuestionsRow leftQuestion rightQuestion =
            let
                leftAnswers =
                    KnowledgeModel.getQuestionAnswers (Question.getUuid leftQuestion) ctx.leftKm

                rightAnswers =
                    KnowledgeModel.getQuestionAnswers (Question.getUuid rightQuestion) ctx.rightKm

                answers =
                    Differ.createDiff .uuid Answer.equalContent leftAnswers rightAnswers

                filteredAnswers =
                    filterCollapsed leftQuestion answers

                leftItemQuestions =
                    KnowledgeModel.getQuestionItemTemplateQuestions (Question.getUuid leftQuestion) ctx.leftKm

                rightItemQuestions =
                    KnowledgeModel.getQuestionItemTemplateQuestions (Question.getUuid rightQuestion) ctx.rightKm

                itemQuestions =
                    Differ.createDiff Question.getUuid Question.equalContent leftItemQuestions rightItemQuestions

                filteredItemQuestions =
                    filterCollapsed leftQuestion itemQuestions

                leftChoices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid leftQuestion) ctx.leftKm

                rightChoices =
                    KnowledgeModel.getQuestionChoices (Question.getUuid rightQuestion) ctx.rightKm

                choices =
                    Differ.createDiff .uuid Choice.equalContent leftChoices rightChoices

                filteredChoices =
                    filterCollapsed leftQuestion choices

                leftReferences =
                    KnowledgeModel.getQuestionReferences (Question.getUuid leftQuestion) ctx.leftKm

                rightReferences =
                    KnowledgeModel.getQuestionReferences (Question.getUuid rightQuestion) ctx.rightKm

                references =
                    Differ.createDiff Reference.getUuid Reference.equalContent leftReferences rightReferences

                filteredReferences =
                    filterCollapsed leftQuestion references

                leftExperts =
                    KnowledgeModel.getQuestionExperts (Question.getUuid leftQuestion) ctx.leftKm

                rightExperts =
                    KnowledgeModel.getQuestionExperts (Question.getUuid rightQuestion) ctx.rightKm

                experts =
                    Differ.createDiff .uuid Expert.equalContent leftExperts rightExperts

                filteredExperts =
                    filterCollapsed leftQuestion experts
            in
            questionRow answers itemQuestions choices references experts
                :: createAnswerTitleRow filteredAnswers
                ++ createAnswerRows filteredAnswers
                ++ createItemQuestionTitleRow filteredItemQuestions
                ++ createItemQuestionRows filteredItemQuestions
                ++ createChoiceTitleRow filteredChoices
                ++ createChoiceRows filteredChoices
                ++ createReferenceTitleRow filteredReferences
                ++ createReferenceRows filteredReferences
                ++ createExpertTitleRow filteredExperts
                ++ createExpertRows filteredExperts
    in
    case questionDiff of
        Differ.Added question ->
            createSingleQuestionRow Differ.Added Differ.Added Differ.Added Differ.Added Differ.Added question ctx.rightKm

        Differ.Removed question ->
            createSingleQuestionRow Differ.Removed Differ.Removed Differ.Removed Differ.Removed Differ.Removed question ctx.leftKm

        Differ.Changed leftQuestion rightQuestion ->
            createBothQuestionsRow leftQuestion rightQuestion

        Differ.NoChange leftQuestion rightQuestion ->
            createBothQuestionsRow leftQuestion rightQuestion


foldAnswer : FoldContext -> Differ.DiffResult Answer -> List (Html Msg)
foldAnswer ctx answerDiff =
    let
        answerCtx =
            addIndentLevel ctx

        answerRow followUpQuestions =
            viewItemRow
                { indentLevel = ctx.indentLevel
                , item = answerDiff
                , icon = faKmAnswer
                , getTitle = GetTitleSimple .label
                , getUuid = .uuid
                , collapsedUuids = ctx.collapsedUuids
                , hasChildren = not (List.isEmpty followUpQuestions)
                , sidePanelWrapper = SidePanel.SidePanelAnswer
                }

        filterCollapsed answer items =
            if Set.member answer.uuid ctx.collapsedUuids then
                []

            else
                items

        createFollowUpQuestionTitleRow questions =
            viewTitleRow
                { title = gettext "Follow-up Questions" ctx.locale
                , indentLevel = answerCtx.indentLevel
                , items = questions
                , item = answerDiff
                }

        createFollowUpQuestionRows questions =
            List.concatMap (foldQuestion answerCtx) questions

        createSingleAnswerRow mapper answer km =
            let
                followUpQuestions =
                    KnowledgeModel.getAnswerFollowupQuestions answer.uuid km
                        |> List.map mapper

                filteredFollowUpQuestions =
                    filterCollapsed answer followUpQuestions
            in
            answerRow followUpQuestions
                :: createFollowUpQuestionTitleRow filteredFollowUpQuestions
                ++ createFollowUpQuestionRows filteredFollowUpQuestions

        createBothAnswersRow leftAnswer rightAnswer =
            let
                leftFollowUpQuestions =
                    KnowledgeModel.getAnswerFollowupQuestions leftAnswer.uuid ctx.leftKm

                rightFollowUpQuestions =
                    KnowledgeModel.getAnswerFollowupQuestions rightAnswer.uuid ctx.rightKm

                followUpQuestions =
                    Differ.createDiff Question.getUuid Question.equalContent leftFollowUpQuestions rightFollowUpQuestions

                filteredFollowUpQuestions =
                    filterCollapsed leftAnswer followUpQuestions
            in
            answerRow followUpQuestions
                :: createFollowUpQuestionTitleRow filteredFollowUpQuestions
                ++ createFollowUpQuestionRows filteredFollowUpQuestions
    in
    case answerDiff of
        Differ.Added answer ->
            createSingleAnswerRow Differ.Added answer ctx.rightKm

        Differ.Removed answer ->
            createSingleAnswerRow Differ.Removed answer ctx.leftKm

        Differ.Changed leftAnswer rightAnswer ->
            createBothAnswersRow leftAnswer rightAnswer

        Differ.NoChange leftAnswer rightAnswer ->
            createBothAnswersRow leftAnswer rightAnswer


foldChoice : FoldContext -> Differ.DiffResult Choice -> List (Html Msg)
foldChoice ctx choiceDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = choiceDiff
        , icon = faKmChoice
        , getTitle = GetTitleSimple .label
        , getUuid = .uuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelChoice
        }
    ]


foldReference : FoldContext -> Differ.DiffResult Reference -> List (Html Msg)
foldReference ctx referenceDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = referenceDiff
        , icon = faKmReference
        , getTitle =
            GetTitleSideBased
                (Reference.getVisibleName (KnowledgeModel.getAllQuestions ctx.leftKm) (KnowledgeModel.getAllResourcePages ctx.leftKm))
                (Reference.getVisibleName (KnowledgeModel.getAllQuestions ctx.rightKm) (KnowledgeModel.getAllResourcePages ctx.rightKm))
        , getUuid = Reference.getUuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelReference
        }
    ]


foldExpert : FoldContext -> Differ.DiffResult Expert -> List (Html Msg)
foldExpert ctx expertDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = expertDiff
        , icon = faKmChapter
        , getTitle = GetTitleSimple Expert.getVisibleName
        , getUuid = .uuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelExpert
        }
    ]


foldMetric : FoldContext -> Differ.DiffResult Metric -> List (Html Msg)
foldMetric ctx metricDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = metricDiff
        , icon = faKmMetric
        , getTitle = GetTitleSimple .title
        , getUuid = .uuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelMetric
        }
    ]


foldPhase : FoldContext -> Differ.DiffResult Phase -> List (Html Msg)
foldPhase ctx phaseDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = phaseDiff
        , icon = faKmPhase
        , getTitle = GetTitleSimple .title
        , getUuid = .uuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelPhase
        }
    ]


foldTag : FoldContext -> Differ.DiffResult Tag -> List (Html Msg)
foldTag ctx tagDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = tagDiff
        , icon = faKmTag
        , getTitle = GetTitleSimple .name
        , getUuid = .uuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelTag
        }
    ]


foldIntegration : FoldContext -> Differ.DiffResult Integration -> List (Html Msg)
foldIntegration ctx integrationDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = integrationDiff
        , icon = faKmIntegration
        , getTitle = GetTitleSimple Integration.getName
        , getUuid = Integration.getUuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelIntegration
        }
    ]


foldResourceCollection : FoldContext -> Differ.DiffResult ResourceCollection -> List (Html Msg)
foldResourceCollection ctx resourceCollectionDiff =
    let
        resourceCollectionCtx =
            addIndentLevel ctx

        resourceCollectionRow resourcePages =
            viewItemRow
                { indentLevel = ctx.indentLevel
                , item = resourceCollectionDiff
                , icon = faKmResourceCollection
                , getTitle = GetTitleSimple .title
                , getUuid = .uuid
                , collapsedUuids = ctx.collapsedUuids
                , hasChildren = not (List.isEmpty resourcePages)
                , sidePanelWrapper = SidePanel.SidePanelResourceCollection
                }

        filterCollapsed resourceCollection items =
            if Set.member resourceCollection.uuid ctx.collapsedUuids then
                []

            else
                items

        createResourcePageTitleRow resourcePages =
            viewTitleRow
                { title = gettext "Resource Pages" ctx.locale
                , indentLevel = resourceCollectionCtx.indentLevel
                , items = resourcePages
                , item = resourceCollectionDiff
                }

        createResourcePageRows resourcePages =
            List.concatMap (foldResourcePage resourceCollectionCtx) resourcePages

        createSingleResourceCollectionRow mapper resourceCollection km =
            let
                resourcePages =
                    KnowledgeModel.getResourceCollectionResourcePages resourceCollection.uuid km
                        |> List.map mapper

                filteredResourcePages =
                    filterCollapsed resourceCollection resourcePages
            in
            resourceCollectionRow resourcePages
                :: createResourcePageTitleRow filteredResourcePages
                ++ createResourcePageRows filteredResourcePages

        createBothResourceCollectionsRow leftResourceCollection rightResourceCollection =
            let
                leftResourcePages =
                    KnowledgeModel.getResourceCollectionResourcePages leftResourceCollection.uuid ctx.leftKm

                rightResourcePages =
                    KnowledgeModel.getResourceCollectionResourcePages rightResourceCollection.uuid ctx.rightKm

                resourcePages =
                    Differ.createDiff .uuid ResourcePage.equalContent leftResourcePages rightResourcePages

                filteredResourcePages =
                    filterCollapsed leftResourceCollection resourcePages
            in
            resourceCollectionRow resourcePages
                :: createResourcePageTitleRow filteredResourcePages
                ++ createResourcePageRows filteredResourcePages
    in
    case resourceCollectionDiff of
        Differ.Added resourceCollection ->
            createSingleResourceCollectionRow Differ.Added resourceCollection ctx.rightKm

        Differ.Removed resourceCollection ->
            createSingleResourceCollectionRow Differ.Removed resourceCollection ctx.leftKm

        Differ.Changed leftResourceCollection rightResourceCollection ->
            createBothResourceCollectionsRow leftResourceCollection rightResourceCollection

        Differ.NoChange leftResourceCollection rightResourceCollection ->
            createBothResourceCollectionsRow leftResourceCollection rightResourceCollection


foldResourcePage : FoldContext -> Differ.DiffResult ResourcePage -> List (Html Msg)
foldResourcePage ctx resourcePageDiff =
    [ viewItemRow
        { indentLevel = ctx.indentLevel
        , item = resourcePageDiff
        , icon = faKmReference
        , getTitle = GetTitleSimple .title
        , getUuid = .uuid
        , collapsedUuids = ctx.collapsedUuids
        , hasChildren = False
        , sidePanelWrapper = SidePanel.SidePanelResourcePage
        }
    ]


type alias ViewItemRowProps a =
    { indentLevel : Int
    , item : Differ.DiffResult a
    , icon : Html Msg
    , getTitle : ViewItemGetTitle a
    , getUuid : a -> String
    , collapsedUuids : Set String
    , hasChildren : Bool
    , sidePanelWrapper : Differ.DiffResult a -> SidePanel.SidePanelState
    }


type ViewItemGetTitle a
    = GetTitleSimple (a -> String)
    | GetTitleSideBased (a -> String) (a -> String)


viewItemRow : ViewItemRowProps a -> Html Msg
viewItemRow props =
    let
        ( getTitleLeft, getTitleRight ) =
            case props.getTitle of
                GetTitleSimple getTitle ->
                    ( getTitle, getTitle )

                GetTitleSideBased getLeft getRight ->
                    ( getLeft, getRight )

        collapseIcon item =
            if props.hasChildren then
                let
                    itemUuid =
                        props.getUuid item
                in
                a
                    [ onClick (ToggleCollapse itemUuid)
                    , class "me-2 text-body"
                    ]
                <|
                    if Set.member itemUuid props.collapsedUuids then
                        [ fas "fa-caret-right fa-fw" ]

                    else
                        [ fas "fa-caret-down fa-fw" ]

            else
                span [ class "me-2" ] [ fas "fa-fw" ]

        iconAndTitle getTitle item =
            div
                [ paddingLeftStyle props.indentLevel
                , class "d-flex align-items-baseline"
                ]
                [ collapseIcon item
                , a [ onClick (SetSidePanel (Just (props.sidePanelWrapper props.item))) ]
                    [ props.icon
                    , span [ class "ms-2" ] [ text (getTitle item) ]
                    ]
                ]
    in
    case props.item of
        Differ.Added item ->
            tr []
                [ td [] []
                , td [ class "table-success" ]
                    [ iconAndTitle getTitleRight item
                    ]
                ]

        Differ.Removed item ->
            tr []
                [ td [ class "table-danger" ]
                    [ iconAndTitle getTitleLeft item
                    ]
                , td [] []
                ]

        Differ.Changed leftItem rightItem ->
            tr []
                [ td [ class "table-warning" ]
                    [ iconAndTitle getTitleLeft leftItem
                    ]
                , td [ class "table-warning" ]
                    [ iconAndTitle getTitleRight rightItem
                    ]
                ]

        Differ.NoChange leftItem rightItem ->
            tr []
                [ td [ class "text-muted" ]
                    [ iconAndTitle getTitleLeft leftItem
                    ]
                , td [ class "text-muted" ]
                    [ iconAndTitle getTitleRight rightItem
                    ]
                ]


type alias ViewTitleRowProps a b =
    { title : String
    , indentLevel : Int
    , items : List a
    , item : Differ.DiffResult b
    }


viewTitleRow : ViewTitleRowProps a b -> List (Html msg)
viewTitleRow props =
    if List.isEmpty props.items then
        []

    else
        let
            titleCell =
                td [ class "table-light w-50" ]
                    [ div [ paddingLeftStyle props.indentLevel, class "text-muted" ]
                        [ text props.title ]
                    ]
        in
        case props.item of
            Differ.Added _ ->
                [ tr []
                    [ td [] []
                    , titleCell
                    ]
                ]

            Differ.Removed _ ->
                [ tr []
                    [ titleCell
                    , td [] []
                    ]
                ]

            Differ.Changed _ _ ->
                [ tr []
                    [ titleCell
                    , titleCell
                    ]
                ]

            Differ.NoChange _ _ ->
                [ tr []
                    [ titleCell, titleCell ]
                ]


paddingLeftStyle : Int -> Html.Attribute msg
paddingLeftStyle indentLevel =
    style "padding-left" (String.fromInt (indentLevel * 35) ++ "px")
