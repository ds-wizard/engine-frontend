module Common.Pages.KMEditor.Migration exposing
    ( Action
    , Model
    , Msg
    , init
    , initialModel
    , update
    , view
    )

{-| Moving a knowledge model editor onto a newer parent, one conflict at a time.

The server drives it: it holds the migration, hands back the next event it
cannot apply on its own, and takes back apply, reject or apply-all. Every answer
reloads the migration, so the page never works out what comes next — it asks.

**What a conflict says.** The page says what is being migrated onto what, and
then, for the event in hand: what kind of change it is, the three ways to answer
it, the fields it sets with what changed marked inside them, and the path down
to the entity it touches.

The path is drawn rather than described, because "in Administrative" does not
say whether that is a chapter, a question inside one, or an answer three levels
down. It is only the lineage — one child per level, no siblings — so it stays a
picture of where you are rather than a copy of the knowledge model.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.KnowledgeModelEditors as KnowledgeModelEditorsApi
import Common.Api.Models.Event as Event exposing (Event)
import Common.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Common.Api.Models.Event.EditIntegrationEventData as EditIntegrationEventData
import Common.Api.Models.Event.EditQuestionEventData as EditQuestionEventData
import Common.Api.Models.Event.EditReferenceEventData as EditReferenceEventData
import Common.Api.Models.Event.EventContent as EventContent exposing (EventContent(..))
import Common.Api.Models.Event.EventField as EventField
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Common.Api.Models.KnowledgeModel.Integration as Integration
import Common.Api.Models.KnowledgeModel.Integration.ApiIntegrationData exposing (ApiIntegrationData)
import Common.Api.Models.KnowledgeModel.Integration.KeyValuePair as KeyValuePair
import Common.Api.Models.KnowledgeModel.Integration.PluginIntegrationData exposing (PluginIntegrationData)
import Common.Api.Models.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question)
import Common.Api.Models.KnowledgeModel.Question.QuestionValidation as QuestionValidation exposing (QuestionValidation)
import Common.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType)
import Common.Api.Models.KnowledgeModel.Reference as Reference
import Common.Api.Models.KnowledgeModelMigration exposing (KnowledgeModelMigration)
import Common.Api.Models.KnowledgeModelMigration.KnowledgeModelMigrationState as MigrationState exposing (KnowledgeModelMigrationState(..))
import Common.Api.Models.KnowledgeModelMigrationResolution as MigrationResolution
import Common.Components.DetailPage as DetailPage
import Common.Components.DiffText as DiffText
import Common.Components.Page as Page
import Common.Data.AppState exposing (AppState)
import Common.Data.NotFound as NotFound
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Routes as Routes
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, div, li, span, strong, text, ul)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.DescriptionList as DescriptionList
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Layout as Layout
import Ui.Page.Message as Message
import Ui.TestId as TestId
import Ui.Text as Text
import Uuid exposing (Uuid)



-- MODEL


type alias Model =
    { uuid : Uuid
    , migration : ActionResult KnowledgeModelMigration

    -- Which of the three actions is in flight, so only that one spins.
    , resolving : Maybe Action
    , error : Maybe String
    }


type Action
    = Apply
    | Reject
    | ApplyAll


initialModel : Uuid -> Model
initialModel uuid =
    { uuid = uuid, migration = Loading, resolving = Nothing, error = Nothing }


init : AppState -> Uuid -> ( Model, Cmd Msg )
init appState uuid =
    ( initialModel uuid, KnowledgeModelEditorsApi.getMigration appState uuid GotMigration )



-- UPDATE


type Msg
    = GotMigration (Result ApiError KnowledgeModelMigration)
    | Resolve Action
    | Resolved (Result ApiError ())


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GotMigration result ->
            RequestHelpers.applyResultOrNotFound
                { setResult = \value m -> { m | migration = value, resolving = Nothing }
                , defaultError = gettext "Unable to get the migration." appState.locale
                , notFoundCmd = UpdateConfig.notFound cfg (NotFound.knowledgeModelMigration appState)
                , notAllowedCmd = UpdateConfig.notAllowed cfg
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        Resolve action ->
            ( { model | resolving = Just action, error = Nothing }
            , Cmd.map cfg.wrapMsg (resolveCmd appState model action)
            )

        Resolved result ->
            case result of
                Ok _ ->
                    -- The server decides what comes next, so the answer is to
                    -- ask it again rather than to work it out here.
                    ( { model | migration = Loading }
                    , Cmd.map cfg.wrapMsg (KnowledgeModelEditorsApi.getMigration appState model.uuid GotMigration)
                    )

                Err error ->
                    ( { model
                        | resolving = Nothing
                        , error = errorMessage (ApiError.toActionResult appState (gettext "Unable to resolve the conflict." appState.locale) error)
                      }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


resolveCmd : AppState -> Model -> Action -> Cmd Msg
resolveCmd appState model action =
    case action of
        ApplyAll ->
            KnowledgeModelEditorsApi.postMigrationConflictApplyAll appState model.uuid Resolved

        _ ->
            case conflictEvent model.migration of
                Just event ->
                    let
                        resolution =
                            if action == Apply then
                                MigrationResolution.apply (Event.getUuid event)

                            else
                                MigrationResolution.reject (Event.getUuid event)
                    in
                    KnowledgeModelEditorsApi.postMigrationConflict appState
                        model.uuid
                        (MigrationResolution.encode resolution)
                        Resolved

                Nothing ->
                    Cmd.none


{-| What an `ActionResult` says went wrong, which is the server's own message
where it sent one and the default otherwise.
-}
errorMessage : ActionResult () -> Maybe String
errorMessage result =
    case result of
        ActionResult.Error message ->
            Just message

        _ ->
            Nothing


conflictEvent : ActionResult KnowledgeModelMigration -> Maybe Event
conflictEvent migration =
    case ActionResult.toMaybe migration |> Maybe.map .state of
        Just (Conflict mbEvent) ->
            mbEvent

        _ ->
            Nothing



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResult appState (viewMigration appState model) model.migration


viewMigration : AppState -> Model -> KnowledgeModelMigration -> Html Msg
viewMigration appState model migration =
    DetailPage.toHtml
        (DetailPage.detailPage
            { title = migration.editorName
            , content = [ viewState appState model migration ]
            }
        )


{-| What is being migrated onto what. The versions are what somebody checking a
migration needs first, and they are not in the page's title.
-}
viewSummary : AppState -> KnowledgeModelMigration -> Html Msg
viewSummary appState migration =
    Text.bodyHtml
        (String.formatHtml
            (gettext "Migration of %s from %s to %s." appState.locale)
            [ strong [] [ text migration.editorName ]
            , Text.code (KnowledgeModelUtils.getPackageId migration.editorPreviousPackage)
            , Text.code (KnowledgeModelUtils.getPackageId migration.targetPackage)
            ]
        )


viewState : AppState -> Model -> KnowledgeModelMigration -> Html Msg
viewState appState model migration =
    let
        withSummary content =
            div [ class "c-km-migration" ] [ viewSummary appState migration, content ]
    in
    case migration.state of
        Conflict (Just event) ->
            withSummary (viewConflict appState model migration event)

        Conflict Nothing ->
            withSummary (Alert.toHtml (Alert.info (gettext "There is nothing left to resolve." appState.locale)))

        Completed ->
            viewCompleted appState migration

        Running ->
            withSummary (Alert.toHtml (Alert.info (gettext "The migration is still running. Refresh the page in a moment." appState.locale)))

        MigrationState.Error ->
            withSummary (Alert.toHtml (Alert.danger (gettext "The migration failed." appState.locale)))


{-| The end of the migration: what was migrated onto what, said once in the
message rather than above it, and the way on, to publishing.
-}
viewCompleted : AppState -> KnowledgeModelMigration -> Html Msg
viewCompleted appState migration =
    Message.messageHtml
        { title = gettext "Migration complete" appState.locale
        , description =
            String.formatHtml
                (gettext "%s has been migrated from %s to %s. Every change has been resolved, and the editor can now be published." appState.locale)
                [ strong [] [ text migration.editorName ]
                , Text.code (KnowledgeModelUtils.getPackageId migration.editorPreviousPackage)
                , Text.code (KnowledgeModelUtils.getPackageId migration.targetPackage)
                ]
        }
        |> Message.withIllustration (Illustration.toHtml Illustration.success)
        |> Message.withAction
            (Button.linkPrimary (gettext "Publish" appState.locale)
                (Routes.toUrl appState (Routes.kmEditorPublish migration.editorUuid))
            )
        |> Message.withinPage
        |> Message.announced
        |> Message.toHtml


viewConflict : AppState -> Model -> KnowledgeModelMigration -> Event -> Html Msg
viewConflict appState model migration event =
    Layout.stack
        [ Text.muted
            (gettext "This change was made to the newer knowledge model. Apply it to keep it, or reject it to leave the editor as it is." appState.locale)
        , case model.error of
            Just error ->
                Alert.toHtml (Alert.danger error)

            Nothing ->
                text ""
        , div [ class "c-km-migration__conflict" ]
            [ div [ class "c-km-migration__event", TestId.attribute (conflictTestId event) ]
                -- The answer comes first. The fields under it can be long, and
                -- a decision you have to scroll past the evidence to make is a
                -- decision made at the bottom of the page.
                -- Rejecting is the one that throws the change away, so it
                -- stands apart from the two that keep it.
                [ div [ class "c-km-migration__actions" ]
                    [ actionButton model Reject (gettext "Reject" appState.locale) (Icon.solid "fa-xmark")
                    , div [ class "ui-inline" ]
                        [ actionButton model Apply (gettext "Apply" appState.locale) (Icon.solid "fa-check")
                        , actionButton model ApplyAll (gettext "Apply all" appState.locale) (Icon.solid "fa-check-double")
                        ]
                    ]
                , Text.label (changeKind appState event)
                , Text.title (entityName appState migration.currentKnowledgeModel event)
                , DescriptionList.toHtml (eventFields appState migration event)
                ]
            , viewPath appState migration event
            ]
        ]


{-| All three actions are disabled while any of them is in flight, and only the
one pressed spins.

Applying is the one most often wanted, so it is the primary. Rejecting throws
the incoming change away and cannot be undone from here, so it takes the danger
role — the outline one, because it is a choice on the page rather than the
confirmation of a destruction.

-}
actionButton : Model -> Action -> String -> Icon.Icon -> Html Msg
actionButton model which label icon =
    let
        style =
            case which of
                Apply ->
                    Button.primary

                Reject ->
                    Button.dangerSecondary

                ApplyAll ->
                    Button.secondary
    in
    Button.toHtml
        (style label (Resolve which)
            |> Button.withIcon icon
            |> Button.withLoading (model.resolving == Just which)
            |> Button.withDisabled (Maybe.isJust model.resolving && model.resolving /= Just which)
            |> Button.withTestId ("km-migration_" ++ actionTestId which)
        )


actionTestId : Action -> String
actionTestId which =
    case which of
        Apply ->
            "apply"

        Reject ->
            "reject"

        ApplyAll ->
            "apply-all"


{-| The conflict says what kind of change it is: `km-migration_conflict_` and
the event's type, `edit-question` for an EditQuestionEvent.
-}
conflictTestId : Event -> String
conflictTestId event =
    EventContent.eventType event.content
        -- Without the "Event" every type ends in.
        |> String.dropRight 5
        |> kebab
        |> (++) "km-migration_conflict_"


kebab : String -> String
kebab =
    String.foldl
        (\char acc ->
            if Char.isUpper char && not (String.isEmpty acc) then
                acc ++ "-" ++ String.fromChar (Char.toLower char)

            else
                acc ++ String.fromChar (Char.toLower char)
        )
        ""



-- WHERE THE ENTITY SITS


{-| The lineage down to the entity the event is about: the knowledge model, then
one child per level, then the entity itself marked with what is happening to it.

Only the path, never a sibling. It is there to say where you are, and a tree of
everything would bury that in everything else.

-}
viewPath : AppState -> KnowledgeModelMigration -> Event -> Html Msg
viewPath appState migration event =
    let
        km =
            migration.currentKnowledgeModel

        parentMap =
            KnowledgeModel.createParentMap km

        ancestors =
            climb appState migration parentMap event.parentUuid [] 0

        leaf =
            { icon = entityIcon appState km event
            , label = entityName appState km event
            , state = Just (changeState appState event)
            }
    in
    div [ class "c-km-migration__path" ]
        [ Text.label (gettext "Where it is" appState.locale)
        , nest (ancestors ++ [ leaf ])
        ]


type alias PathNode =
    { icon : Icon.Icon
    , label : String
    , state : Maybe ( String, String )
    }


{-| Up from the entity's parent to the knowledge model, gathering what each one
is called. The depth is capped because the parent map comes from the server and
a cycle in it would otherwise hang the page rather than draw a wrong tree.
-}
climb : AppState -> KnowledgeModelMigration -> KnowledgeModel.ParentMap -> String -> List PathNode -> Int -> List PathNode
climb appState migration parentMap uuid found depth =
    if depth > 12 || String.isEmpty uuid then
        found

    else if uuid == Uuid.toString migration.currentKnowledgeModel.uuid then
        { icon = Icon.solid "fa-sitemap", label = migration.editorName, state = Nothing } :: found

    else
        case namedEntity appState migration.currentKnowledgeModel uuid of
            Just node ->
                climb appState migration parentMap (KnowledgeModel.getParent parentMap uuid) (node :: found) (depth + 1)

            Nothing ->
                found


{-| What an entity in the knowledge model is, and what it is called. The kinds
are tried in turn because a uuid says nothing about which dictionary it is in.
-}
namedEntity : AppState -> KnowledgeModel.KnowledgeModel -> String -> Maybe PathNode
namedEntity appState km uuid =
    let
        named icon name =
            Just
                { icon = icon
                , label =
                    if String.isEmpty name then
                        gettext "Untitled" appState.locale

                    else
                        name
                , state = Nothing
                }
    in
    [ Maybe.map (\entity -> named (Icon.regular "fa-file") entity.title) (KnowledgeModel.getChapter uuid km)
    , Maybe.map (\entity -> named (Icon.regular "fa-comment") (Question.getTitle entity)) (KnowledgeModel.getQuestion uuid km)
    , Maybe.map (\entity -> named (Icon.regular "fa-circle-dot") entity.label) (KnowledgeModel.getAnswer uuid km)
    , Maybe.map (\entity -> named (Icon.regular "fa-square-check") entity.label) (KnowledgeModel.getChoice uuid km)
    , Maybe.map (\entity -> named (Icon.solid "fa-chart-column") entity.title) (KnowledgeModel.getMetric uuid km)
    , Maybe.map (\entity -> named (Icon.regular "fa-clock") entity.title) (KnowledgeModel.getPhase uuid km)
    , Maybe.map (\entity -> named (Icon.solid "fa-tag") entity.name) (KnowledgeModel.getTag uuid km)
    , Maybe.map (\entity -> named (Icon.solid "fa-right-left") (Integration.getVisibleName entity)) (KnowledgeModel.getIntegration uuid km)
    , Maybe.map (\entity -> named (Icon.solid "fa-book") entity.title) (KnowledgeModel.getResourceCollection uuid km)
    , Maybe.map (\entity -> named (Icon.regular "fa-file-lines") entity.title) (KnowledgeModel.getResourcePage uuid km)
    ]
        |> Maybe.values
        |> List.head
        |> Maybe.join


{-| One level inside the next, down to the entity. A nested list, because that
is what a path is — and a screen reader reads the nesting as the depth.
-}
nest : List PathNode -> Html Msg
nest nodes =
    case nodes of
        [] ->
            text ""

        node :: rest ->
            ul [ class "c-km-migration__path-list" ]
                [ li []
                    (span [ class (pathClass node) ]
                        (Icon.toHtml node.icon
                            :: text node.label
                            :: (case node.state of
                                    Just ( _, label ) ->
                                        [ Text.visuallyHiddenInline (": " ++ label) ]

                                    Nothing ->
                                        []
                               )
                        )
                        :: (case rest of
                                [] ->
                                    []

                                _ ->
                                    [ nest rest ]
                           )
                    )
                ]


pathClass : PathNode -> String
pathClass node =
    case node.state of
        Just ( modifier, _ ) ->
            "c-km-migration__path-node is-" ++ modifier

        Nothing ->
            "c-km-migration__path-node"


{-| The fields the event sets, with what changed marked inside them.

An **edit** carries only the fields it touches — every other one is `Nothing` —
so only those are shown, and a reader is not asked to scan a form's worth of
values for the two that moved. An **add** is every field arriving and a
**delete** is every field going, which is the same diff with one side empty.

Every value is drawn as a string and diffed as one, whatever it is underneath: a
list of uuids becomes the names those uuids stand for, a dictionary becomes its
pairs, a switch becomes Yes or No. A uuid on the screen tells nobody anything.

-}
eventFields : AppState -> KnowledgeModelMigration -> Event -> List (DescriptionList.Entry Msg)
eventFields appState migration event =
    let
        km =
            migration.currentKnowledgeModel

        locale =
            appState.locale

        -- The key names the field for the tests, in every language.
        field key label before after =
            DescriptionList.withHtml
                { term = label
                , description =
                    [ if String.isEmpty before && String.isEmpty after then
                        Text.muted (gettext "Not set" locale)

                      else
                        DiffText.words before after
                    ]
                }
                |> DescriptionList.withTestId ("km-migration_field_" ++ key)

        arriving key label value =
            field key label "" value

        going key label value =
            field key label value ""

        editing key label before after =
            case after of
                Just value ->
                    [ field key label before value ]

                Nothing ->
                    []

        -- The entity as it stands, which is what an edit changes it from and
        -- what a delete takes away.
        current get =
            Maybe.withDefault "" (get km)

        question =
            KnowledgeModel.getQuestion event.entityUuid km

        integration =
            KnowledgeModel.getIntegration event.entityUuid km

        -- The fields every question has, whatever kind it is.
        questionCommon before title text phase tags references experts =
            editing "title" (gettext "Title" locale) (Maybe.unwrap "" Question.getTitle before) title
                ++ editing "text" (gettext "Text" locale) (Maybe.withDefault "" (Maybe.andThen Question.getText before)) text
                ++ editing "required-phase"
                    (gettext "Required phase" locale)
                    (names km (maybeList (Maybe.andThen Question.getRequiredPhaseUuid before)))
                    (Maybe.map (\v -> names km (maybeList v)) phase)
                ++ editing "question-tags" (gettext "Question tags" locale) (names km (Maybe.unwrap [] Question.getTagUuids before)) (Maybe.map (names km) tags)
                ++ editing "references" (gettext "References" locale) (names km (Maybe.unwrap [] Question.getReferenceUuids before)) (Maybe.map (names km) references)
                ++ editing "experts" (gettext "Experts" locale) (names km (Maybe.unwrap [] Question.getExpertUuids before)) (Maybe.map (names km) experts)
    in
    case event.content of
        AddKnowledgeModelEvent data ->
            [ arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations) ]

        EditKnowledgeModelEvent data ->
            editing "chapters" (gettext "Chapters" locale) (names km km.chapterUuids) (Maybe.map (names km) (EventField.getValue data.chapterUuids))
                ++ editing "metrics" (gettext "Metrics" locale) (names km km.metricUuids) (Maybe.map (names km) (EventField.getValue data.metricUuids))
                ++ editing "phases" (gettext "Phases" locale) (names km km.phaseUuids) (Maybe.map (names km) (EventField.getValue data.phaseUuids))
                ++ editing "question-tags" (gettext "Question tags" locale) (names km km.tagUuids) (Maybe.map (names km) (EventField.getValue data.tagUuids))
                ++ editing "integrations" (gettext "Integrations" locale) (names km km.integrationUuids) (Maybe.map (names km) (EventField.getValue data.integrationUuids))
                ++ editing "resource-collections" (gettext "Resource collections" locale) (names km km.resourceCollectionUuids) (Maybe.map (names km) (EventField.getValue data.resourceCollectionUuids))
                ++ editingAnnotations locale field km event data.annotations

        AddChapterEvent data ->
            [ arriving "title" (gettext "Title" locale) data.title
            , arriving "text" (gettext "Text" locale) (Maybe.withDefault "" data.text)
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditChapterEvent data ->
            editing "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getChapter event.entityUuid k))) (EventField.getValue data.title)
                ++ editing "text" (gettext "Text" locale) (current (\k -> Maybe.andThen .text (KnowledgeModel.getChapter event.entityUuid k))) (Maybe.map (Maybe.withDefault "") (EventField.getValue data.text))
                ++ editing "questions" (gettext "Questions" locale) (names km (Maybe.unwrap [] .questionUuids (KnowledgeModel.getChapter event.entityUuid km))) (Maybe.map (names km) (EventField.getValue data.questionUuids))
                ++ editingAnnotations locale field km event data.annotations

        DeleteChapterEvent ->
            [ going "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getChapter event.entityUuid k)))
            , going "text" (gettext "Text" locale) (current (\k -> Maybe.andThen .text (KnowledgeModel.getChapter event.entityUuid k)))
            , going "questions" (gettext "Questions" locale) (names km (Maybe.unwrap [] .questionUuids (KnowledgeModel.getChapter event.entityUuid km)))
            ]

        AddQuestionEvent data ->
            let
                common =
                    [ arriving "title" (gettext "Title" locale) (AddQuestionEventData.map .title .title .title .title .title .title .title data)
                    , arriving "text" (gettext "Text" locale) (Maybe.withDefault "" (AddQuestionEventData.map .text .text .text .text .text .text .text data))
                    , arriving "required-phase" (gettext "Required phase" locale) (names km (maybeList (AddQuestionEventData.map .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid data)))
                    , arriving "question-tags" (gettext "Question tags" locale) (names km (AddQuestionEventData.map .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids data))
                    ]
            in
            common
                ++ AddQuestionEventData.map (always []) (always []) (addValue locale arriving) (addIntegration locale km arriving) (always []) (addItemSelect locale km arriving) (addFile locale arriving) data
                ++ [ arriving "annotations"
                        (gettext "Annotations" locale)
                        (annotations (AddQuestionEventData.map .annotations .annotations .annotations .annotations .annotations .annotations .annotations data))
                   ]

        EditQuestionEvent data ->
            questionCommon question
                (EventField.getValue (EditQuestionEventData.map .title .title .title .title .title .title .title data))
                (Maybe.map (Maybe.withDefault "") (EventField.getValue (EditQuestionEventData.map .text .text .text .text .text .text .text data)))
                (EventField.getValue (EditQuestionEventData.map .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid .requiredPhaseUuid data))
                (EventField.getValue (EditQuestionEventData.map .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids data))
                (EventField.getValue (EditQuestionEventData.map .referenceUuids .referenceUuids .referenceUuids .referenceUuids .referenceUuids .referenceUuids .referenceUuids data))
                (EventField.getValue (EditQuestionEventData.map .expertUuids .expertUuids .expertUuids .expertUuids .expertUuids .expertUuids .expertUuids data))
                ++ EditQuestionEventData.map
                    (editOptions locale km editing question)
                    (editList locale km editing question)
                    (editValue locale editing question)
                    (editIntegration locale km editing question)
                    (editMultiChoice locale km editing question)
                    (editItemSelect locale km editing question)
                    (editFile locale editing question)
                    data
                ++ editingAnnotations locale field km event (EditQuestionEventData.map .annotations .annotations .annotations .annotations .annotations .annotations .annotations data)

        DeleteQuestionEvent ->
            [ going "title" (gettext "Title" locale) (Maybe.unwrap "" Question.getTitle question)
            , going "text" (gettext "Text" locale) (Maybe.withDefault "" (Maybe.andThen Question.getText question))
            , going "question-tags" (gettext "Question tags" locale) (names km (Maybe.unwrap [] Question.getTagUuids question))
            ]

        AddAnswerEvent data ->
            [ arriving "label" (gettext "Label" locale) data.label
            , arriving "advice" (gettext "Advice" locale) (Maybe.withDefault "" data.advice)
            , arriving "metrics" (gettext "Metrics" locale) (measures locale km data.metricMeasures)
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditAnswerEvent data ->
            editing "label" (gettext "Label" locale) (current (\k -> Maybe.map .label (KnowledgeModel.getAnswer event.entityUuid k))) (EventField.getValue data.label)
                ++ editing "advice" (gettext "Advice" locale) (current (\k -> Maybe.andThen .advice (KnowledgeModel.getAnswer event.entityUuid k))) (Maybe.map (Maybe.withDefault "") (EventField.getValue data.advice))
                ++ editing "metrics" (gettext "Metrics" locale) (measures locale km (Maybe.unwrap [] .metricMeasures (KnowledgeModel.getAnswer event.entityUuid km))) (Maybe.map (measures locale km) (EventField.getValue data.metricMeasures))
                ++ editing "follow-up-questions" (gettext "Follow-up questions" locale) (names km (Maybe.unwrap [] .followUpUuids (KnowledgeModel.getAnswer event.entityUuid km))) (Maybe.map (names km) (EventField.getValue data.followUpUuids))
                ++ editingAnnotations locale field km event data.annotations

        DeleteAnswerEvent ->
            [ going "label" (gettext "Label" locale) (current (\k -> Maybe.map .label (KnowledgeModel.getAnswer event.entityUuid k)))
            , going "advice" (gettext "Advice" locale) (current (\k -> Maybe.andThen .advice (KnowledgeModel.getAnswer event.entityUuid k)))
            ]

        AddChoiceEvent data ->
            [ arriving "label" (gettext "Label" locale) data.label
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditChoiceEvent data ->
            editing "label" (gettext "Label" locale) (current (\k -> Maybe.map .label (KnowledgeModel.getChoice event.entityUuid k))) (EventField.getValue data.label)
                ++ editingAnnotations locale field km event data.annotations

        DeleteChoiceEvent ->
            [ going "label" (gettext "Label" locale) (current (\k -> Maybe.map .label (KnowledgeModel.getChoice event.entityUuid k))) ]

        AddExpertEvent data ->
            [ arriving "name" (gettext "Name" locale) data.name
            , arriving "email" (gettext "Email" locale) data.email
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditExpertEvent data ->
            editing "name" (gettext "Name" locale) (current (\k -> Maybe.map .name (KnowledgeModel.getExpert event.entityUuid k))) (EventField.getValue data.name)
                ++ editing "email" (gettext "Email" locale) (current (\k -> Maybe.map .email (KnowledgeModel.getExpert event.entityUuid k))) (EventField.getValue data.email)
                ++ editingAnnotations locale field km event data.annotations

        DeleteExpertEvent ->
            [ going "name" (gettext "Name" locale) (current (\k -> Maybe.map .name (KnowledgeModel.getExpert event.entityUuid k)))
            , going "email" (gettext "Email" locale) (current (\k -> Maybe.map .email (KnowledgeModel.getExpert event.entityUuid k)))
            ]

        AddReferenceEvent data ->
            AddReferenceEventData.map
                (\d -> [ arriving "resource-page" (gettext "Resource page" locale) (names km (maybeList d.resourcePageUuid)), arriving "annotations" (gettext "Annotations" locale) (annotations d.annotations) ])
                (\d -> [ arriving "url" (gettext "URL" locale) d.url, arriving "label" (gettext "Label" locale) d.label, arriving "annotations" (gettext "Annotations" locale) (annotations d.annotations) ])
                (\d -> [ arriving "target" (gettext "Target" locale) (names km [ d.targetUuid ]), arriving "description" (gettext "Description" locale) d.description, arriving "annotations" (gettext "Annotations" locale) (annotations d.annotations) ])
                data

        EditReferenceEvent data ->
            let
                reference =
                    KnowledgeModel.getReference event.entityUuid km
            in
            EditReferenceEventData.map
                (\d ->
                    editing "resource-page"
                        (gettext "Resource page" locale)
                        (names km (maybeList (Maybe.andThen referenceResourcePage reference)))
                        (Maybe.map (\v -> names km (maybeList v)) (EventField.getValue d.resourcePageUuid))
                        ++ editingAnnotations locale field km event d.annotations
                )
                (\d ->
                    editing "url" (gettext "URL" locale) (Maybe.withDefault "" (Maybe.andThen referenceUrl reference)) (EventField.getValue d.url)
                        ++ editing "label" (gettext "Label" locale) (Maybe.withDefault "" (Maybe.andThen referenceLabel reference)) (EventField.getValue d.label)
                        ++ editingAnnotations locale field km event d.annotations
                )
                (\d ->
                    editing "target" (gettext "Target" locale) (names km (maybeList (Maybe.andThen referenceTarget reference))) (Maybe.map (\v -> names km [ v ]) (EventField.getValue d.targetUuid))
                        ++ editing "description" (gettext "Description" locale) (Maybe.withDefault "" (Maybe.andThen referenceDescription reference)) (EventField.getValue d.description)
                        ++ editingAnnotations locale field km event d.annotations
                )
                data

        DeleteReferenceEvent ->
            [ going "reference" (gettext "Reference" locale) (entityName appState km event) ]

        AddMetricEvent data ->
            [ arriving "title" (gettext "Title" locale) data.title
            , arriving "abbreviation" (gettext "Abbreviation" locale) (Maybe.withDefault "" data.abbreviation)
            , arriving "description" (gettext "Description" locale) (Maybe.withDefault "" data.description)
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditMetricEvent data ->
            editing "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getMetric event.entityUuid k))) (EventField.getValue data.title)
                ++ editing "abbreviation" (gettext "Abbreviation" locale) (current (\k -> Maybe.andThen .abbreviation (KnowledgeModel.getMetric event.entityUuid k))) (Maybe.map (Maybe.withDefault "") (EventField.getValue data.abbreviation))
                ++ editing "description" (gettext "Description" locale) (current (\k -> Maybe.andThen .description (KnowledgeModel.getMetric event.entityUuid k))) (Maybe.map (Maybe.withDefault "") (EventField.getValue data.description))
                ++ editingAnnotations locale field km event data.annotations

        DeleteMetricEvent ->
            [ going "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getMetric event.entityUuid k)))
            , going "abbreviation" (gettext "Abbreviation" locale) (current (\k -> Maybe.andThen .abbreviation (KnowledgeModel.getMetric event.entityUuid k)))
            ]

        AddPhaseEvent data ->
            [ arriving "title" (gettext "Title" locale) data.title
            , arriving "description" (gettext "Description" locale) (Maybe.withDefault "" data.description)
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditPhaseEvent data ->
            editing "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getPhase event.entityUuid k))) (EventField.getValue data.title)
                ++ editing "description" (gettext "Description" locale) (current (\k -> Maybe.andThen .description (KnowledgeModel.getPhase event.entityUuid k))) (Maybe.map (Maybe.withDefault "") (EventField.getValue data.description))
                ++ editingAnnotations locale field km event data.annotations

        DeletePhaseEvent ->
            [ going "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getPhase event.entityUuid k)))
            , going "description" (gettext "Description" locale) (current (\k -> Maybe.andThen .description (KnowledgeModel.getPhase event.entityUuid k)))
            ]

        AddTagEvent data ->
            [ arriving "name" (gettext "Name" locale) data.name
            , arriving "description" (gettext "Description" locale) (Maybe.withDefault "" data.description)
            , arriving "color" (gettext "Color" locale) data.color
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditTagEvent data ->
            editing "name" (gettext "Name" locale) (current (\k -> Maybe.map .name (KnowledgeModel.getTag event.entityUuid k))) (EventField.getValue data.name)
                ++ editing "description" (gettext "Description" locale) (current (\k -> Maybe.andThen .description (KnowledgeModel.getTag event.entityUuid k))) (Maybe.map (Maybe.withDefault "") (EventField.getValue data.description))
                ++ editing "color" (gettext "Color" locale) (current (\k -> Maybe.map .color (KnowledgeModel.getTag event.entityUuid k))) (EventField.getValue data.color)
                ++ editingAnnotations locale field km event data.annotations

        DeleteTagEvent ->
            [ going "name" (gettext "Name" locale) (current (\k -> Maybe.map .name (KnowledgeModel.getTag event.entityUuid k)))
            , going "description" (gettext "Description" locale) (current (\k -> Maybe.andThen .description (KnowledgeModel.getTag event.entityUuid k)))
            ]

        AddIntegrationEvent data ->
            AddIntegrationEventData.map
                (\d ->
                    [ arriving "name" (gettext "Name" locale) d.name
                    , arriving "variables" (gettext "Variables" locale) (String.join ", " d.variables)
                    , arriving "request-method" (gettext "Request method" locale) d.requestMethod
                    , arriving "request-url" (gettext "Request URL" locale) d.requestUrl
                    , arriving "request-headers" (gettext "Request headers" locale) (pairs d.requestHeaders)
                    , arriving "request-body" (gettext "Request body" locale) (Maybe.withDefault "" d.requestBody)
                    , arriving "allow-empty-search" (gettext "Allow empty search" locale) (yesNo locale d.requestAllowEmptySearch)
                    , arriving "allow-custom-reply" (gettext "Allow custom reply" locale) (yesNo locale d.allowCustomReply)
                    , arriving "response-list-field" (gettext "Response list field" locale) (Maybe.withDefault "" d.responseListField)
                    , arriving "response-item-template" (gettext "Response item template" locale) d.responseItemTemplate
                    , arriving "response-item-template-for-selection" (gettext "Response item template for selection" locale) (Maybe.withDefault "" d.responseItemTemplateForSelection)
                    , arriving "test-search-query" (gettext "Test search query" locale) d.testQ
                    , arriving "test-variables" (gettext "Test variables" locale) (dict d.testVariables)
                    , arriving "annotations" (gettext "Annotations" locale) (annotations d.annotations)
                    ]
                )
                (\d ->
                    [ arriving "name" (gettext "Name" locale) d.name
                    , arriving "plugin" (gettext "Plugin" locale) d.pluginUuid
                    , arriving "plugin-integration" (gettext "Plugin integration" locale) d.pluginIntegrationId
                    , arriving "plugin-settings" (gettext "Plugin settings" locale) d.pluginIntegrationSettings
                    , arriving "annotations" (gettext "Annotations" locale) (annotations d.annotations)
                    ]
                )
                data

        EditIntegrationEvent data ->
            EditIntegrationEventData.map
                (\d ->
                    editing "name" (gettext "Name" locale) (Maybe.unwrap "" Integration.getName integration) (EventField.getValue d.name)
                        ++ editing "variables" (gettext "Variables" locale) (String.join ", " (Maybe.unwrap [] Integration.getVariables integration)) (Maybe.map (String.join ", ") (EventField.getValue d.variables))
                        ++ editing "request-method" (gettext "Request method" locale) (apiValue integration .requestMethod "") (EventField.getValue d.requestMethod)
                        ++ editing "request-url" (gettext "Request URL" locale) (apiValue integration .requestUrl "") (EventField.getValue d.requestUrl)
                        ++ editing "request-headers" (gettext "Request headers" locale) (pairs (apiValue integration .requestHeaders [])) (Maybe.map pairs (EventField.getValue d.requestHeaders))
                        ++ editing "request-body" (gettext "Request body" locale) (Maybe.withDefault "" (apiValue integration .requestBody Nothing)) (Maybe.map (Maybe.withDefault "") (EventField.getValue d.requestBody))
                        ++ editing "allow-empty-search" (gettext "Allow empty search" locale) (yesNo locale (apiValue integration .requestAllowEmptySearch False)) (Maybe.map (yesNo locale) (EventField.getValue d.requestAllowEmptySearch))
                        ++ editing "allow-custom-reply" (gettext "Allow custom reply" locale) (yesNo locale (apiValue integration .allowCustomReply False)) (Maybe.map (yesNo locale) (EventField.getValue d.allowCustomReply))
                        ++ editing "response-list-field" (gettext "Response list field" locale) (Maybe.withDefault "" (apiValue integration .responseListField Nothing)) (Maybe.map (Maybe.withDefault "") (EventField.getValue d.responseListField))
                        ++ editing "response-item-template" (gettext "Response item template" locale) (apiValue integration .responseItemTemplate "") (EventField.getValue d.responseItemTemplate)
                        ++ editing "response-item-template-for-selection" (gettext "Response item template for selection" locale) (Maybe.withDefault "" (apiValue integration .responseItemTemplateForSelection Nothing)) (Maybe.map (Maybe.withDefault "") (EventField.getValue d.responseItemTemplateForSelection))
                        ++ editing "test-search-query" (gettext "Test search query" locale) (apiValue integration .testQ "") (EventField.getValue d.testQ)
                        ++ editing "test-variables" (gettext "Test variables" locale) (dict (apiValue integration .testVariables Dict.empty)) (Maybe.map dict (EventField.getValue d.testVariables))
                        ++ editingAnnotations locale field km event d.annotations
                )
                (\d ->
                    editing "name" (gettext "Name" locale) (Maybe.unwrap "" Integration.getName integration) (EventField.getValue d.name)
                        ++ editing "plugin" (gettext "Plugin" locale) (pluginValue integration .pluginUuid) (EventField.getValue d.pluginUuid)
                        ++ editing "plugin-integration" (gettext "Plugin integration" locale) (pluginValue integration .pluginIntegrationId) (EventField.getValue d.pluginIntegrationId)
                        ++ editing "plugin-settings" (gettext "Plugin settings" locale) (pluginValue integration .pluginIntegrationSettings) (EventField.getValue d.pluginIntegrationSettings)
                        ++ editingAnnotations locale field km event d.annotations
                )
                data

        DeleteIntegrationEvent ->
            [ going "name" (gettext "Name" locale) (Maybe.unwrap "" Integration.getName integration) ]

        AddResourceCollectionEvent data ->
            [ arriving "title" (gettext "Title" locale) data.title
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditResourceCollectionEvent data ->
            editing "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getResourceCollection event.entityUuid k))) (EventField.getValue data.title)
                ++ editing "resource-pages" (gettext "Resource pages" locale) (names km (Maybe.unwrap [] .resourcePageUuids (KnowledgeModel.getResourceCollection event.entityUuid km))) (Maybe.map (names km) (EventField.getValue data.resourcePageUuids))
                ++ editingAnnotations locale field km event data.annotations

        DeleteResourceCollectionEvent ->
            [ going "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getResourceCollection event.entityUuid k)))
            , going "resource-pages" (gettext "Resource pages" locale) (names km (Maybe.unwrap [] .resourcePageUuids (KnowledgeModel.getResourceCollection event.entityUuid km)))
            ]

        AddResourcePageEvent data ->
            [ arriving "title" (gettext "Title" locale) data.title
            , arriving "content" (gettext "Content" locale) data.content
            , arriving "annotations" (gettext "Annotations" locale) (annotations data.annotations)
            ]

        EditResourcePageEvent data ->
            editing "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getResourcePage event.entityUuid k))) (EventField.getValue data.title)
                ++ editing "content" (gettext "Content" locale) (current (\k -> Maybe.map .content (KnowledgeModel.getResourcePage event.entityUuid k))) (EventField.getValue data.content)
                ++ editingAnnotations locale field km event data.annotations

        DeleteResourcePageEvent ->
            [ going "title" (gettext "Title" locale) (current (\k -> Maybe.map .title (KnowledgeModel.getResourcePage event.entityUuid k)))
            , going "content" (gettext "Content" locale) (current (\k -> Maybe.map .content (KnowledgeModel.getResourcePage event.entityUuid k)))
            ]

        MoveQuestionEvent data ->
            [ field "moving-to" (gettext "Moving to" locale) "" (names km [ data.targetUuid ]) ]

        MoveAnswerEvent data ->
            [ field "moving-to" (gettext "Moving to" locale) "" (names km [ data.targetUuid ]) ]

        MoveChoiceEvent data ->
            [ field "moving-to" (gettext "Moving to" locale) "" (names km [ data.targetUuid ]) ]

        MoveReferenceEvent data ->
            [ field "moving-to" (gettext "Moving to" locale) "" (names km [ data.targetUuid ]) ]

        MoveExpertEvent data ->
            [ field "moving-to" (gettext "Moving to" locale) "" (names km [ data.targetUuid ]) ]



-- WHAT ONE KIND OF QUESTION HAS THAT ANOTHER DOES NOT


addValue : Gettext.Locale -> (String -> String -> String -> DescriptionList.Entry Msg) -> { a | valueType : QuestionValueType, validations : List QuestionValidation } -> List (DescriptionList.Entry Msg)
addValue locale arriving data =
    [ arriving "value-type" (gettext "Value type" locale) (QuestionValueType.toReadableString locale data.valueType)
    , arriving "validations" (gettext "Validations" locale) (validations data.validations)
    ]


addIntegration : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> (String -> String -> String -> DescriptionList.Entry Msg) -> { a | integrationUuid : String, variables : Dict String String } -> List (DescriptionList.Entry Msg)
addIntegration locale km arriving data =
    [ arriving "integration" (gettext "Integration" locale) (nameOf km data.integrationUuid)
    , arriving "variables" (gettext "Variables" locale) (dict data.variables)
    ]


addItemSelect : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> (String -> String -> String -> DescriptionList.Entry Msg) -> { a | listQuestionUuid : Maybe String } -> List (DescriptionList.Entry Msg)
addItemSelect locale km arriving data =
    [ arriving "list-question" (gettext "List question" locale) (names km (maybeList data.listQuestionUuid)) ]


addFile : Gettext.Locale -> (String -> String -> String -> DescriptionList.Entry Msg) -> { a | maxSize : Maybe Int, fileTypes : Maybe String } -> List (DescriptionList.Entry Msg)
addFile locale arriving data =
    [ arriving "maximum-size" (gettext "Maximum size" locale) (Maybe.unwrap "" ByteUnits.toReadable data.maxSize)
    , arriving "file-types" (gettext "File types" locale) (Maybe.withDefault "" data.fileTypes)
    ]


type alias Editing =
    String -> String -> String -> Maybe String -> List (DescriptionList.Entry Msg)


editOptions : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> Editing -> Maybe Question -> { a | answerUuids : EventField.EventField (List String) } -> List (DescriptionList.Entry Msg)
editOptions locale km editing question data =
    editing "answers"
        (gettext "Answers" locale)
        (names km (Maybe.unwrap [] Question.getAnswerUuids question))
        (Maybe.map (names km) (EventField.getValue data.answerUuids))


editList : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> Editing -> Maybe Question -> { a | itemTemplateQuestionUuids : EventField.EventField (List String) } -> List (DescriptionList.Entry Msg)
editList locale km editing question data =
    editing "item-questions"
        (gettext "Item questions" locale)
        (names km (Maybe.unwrap [] Question.getItemTemplateQuestionUuids question))
        (Maybe.map (names km) (EventField.getValue data.itemTemplateQuestionUuids))


editMultiChoice : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> Editing -> Maybe Question -> { a | choiceUuids : EventField.EventField (List String) } -> List (DescriptionList.Entry Msg)
editMultiChoice locale km editing question data =
    editing "choices"
        (gettext "Choices" locale)
        (names km (Maybe.unwrap [] Question.getChoiceUuids question))
        (Maybe.map (names km) (EventField.getValue data.choiceUuids))


editValue : Gettext.Locale -> Editing -> Maybe Question -> { a | valueType : EventField.EventField QuestionValueType, validations : EventField.EventField (List QuestionValidation) } -> List (DescriptionList.Entry Msg)
editValue locale editing question data =
    editing "value-type"
        (gettext "Value type" locale)
        (Maybe.unwrap "" (QuestionValueType.toReadableString locale) (Maybe.andThen Question.getValueType question))
        (Maybe.map (QuestionValueType.toReadableString locale) (EventField.getValue data.valueType))
        ++ editing "validations"
            (gettext "Validations" locale)
            (validations (Maybe.withDefault [] (Maybe.andThen Question.getValidations question)))
            (Maybe.map validations (EventField.getValue data.validations))


editIntegration : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> Editing -> Maybe Question -> { a | integrationUuid : EventField.EventField String, variables : EventField.EventField (Dict String String) } -> List (DescriptionList.Entry Msg)
editIntegration locale km editing question data =
    editing "integration"
        (gettext "Integration" locale)
        (Maybe.unwrap "" (nameOf km) (Maybe.andThen Question.getIntegrationUuid question))
        (Maybe.map (nameOf km) (EventField.getValue data.integrationUuid))
        ++ editing "variables"
            (gettext "Variables" locale)
            (dict (Maybe.withDefault Dict.empty (Maybe.andThen Question.getVariables question)))
            (Maybe.map dict (EventField.getValue data.variables))


editItemSelect : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> Editing -> Maybe Question -> { a | listQuestionUuid : EventField.EventField (Maybe String) } -> List (DescriptionList.Entry Msg)
editItemSelect locale km editing question data =
    editing "list-question"
        (gettext "List question" locale)
        (names km (maybeList (Maybe.andThen Question.getListQuestionUuid question)))
        (Maybe.map (\value -> names km (maybeList value)) (EventField.getValue data.listQuestionUuid))


editFile : Gettext.Locale -> Editing -> Maybe Question -> { a | maxSize : EventField.EventField (Maybe Int), fileTypes : EventField.EventField (Maybe String) } -> List (DescriptionList.Entry Msg)
editFile locale editing question data =
    editing "maximum-size"
        (gettext "Maximum size" locale)
        (Maybe.unwrap "" ByteUnits.toReadable (Maybe.andThen Question.getMaxSize question))
        (Maybe.map (Maybe.withDefault "" << Maybe.map ByteUnits.toReadable) (EventField.getValue data.maxSize))
        ++ editing "file-types"
            (gettext "File types" locale)
            (Maybe.withDefault "" (Maybe.andThen Question.getFileTypes question))
            (Maybe.map (Maybe.withDefault "") (EventField.getValue data.fileTypes))



-- WHAT A VALUE LOOKS LIKE
--
-- Everything is drawn as a string and diffed as one. A uuid on the screen tells
-- nobody anything, so a list of them becomes the names they stand for.


maybeList : Maybe a -> List a
maybeList value =
    case value of
        Just one ->
            [ one ]

        Nothing ->
            []


{-| The names the uuids stand for, in the order given.
-}
names : KnowledgeModel.KnowledgeModel -> List String -> String
names km uuids =
    String.join ", " (List.map (nameOf km) uuids)


nameOf : KnowledgeModel.KnowledgeModel -> String -> String
nameOf km uuid =
    knownName km uuid
        -- Something the knowledge model does not have yet: an entity this very
        -- migration is adding. Its uuid is all there is to say.
        |> Maybe.withDefault uuid


{-| What the knowledge model calls the entity, if it has it.
-}
knownName : KnowledgeModel.KnowledgeModel -> String -> Maybe String
knownName km uuid =
    [ Maybe.map .title (KnowledgeModel.getChapter uuid km)
    , Maybe.map Question.getTitle (KnowledgeModel.getQuestion uuid km)
    , Maybe.map .label (KnowledgeModel.getAnswer uuid km)
    , Maybe.map .label (KnowledgeModel.getChoice uuid km)
    , Maybe.map .title (KnowledgeModel.getMetric uuid km)
    , Maybe.map .title (KnowledgeModel.getPhase uuid km)
    , Maybe.map .name (KnowledgeModel.getTag uuid km)
    , Maybe.map Integration.getVisibleName (KnowledgeModel.getIntegration uuid km)
    , Maybe.map .name (KnowledgeModel.getExpert uuid km)
    , Maybe.map .title (KnowledgeModel.getResourceCollection uuid km)
    , Maybe.map .title (KnowledgeModel.getResourcePage uuid km)
    , Maybe.map (Reference.getVisibleName (KnowledgeModel.getAllQuestions km) (KnowledgeModel.getAllResourcePages km)) (KnowledgeModel.getReference uuid km)
    ]
        |> Maybe.values
        |> List.head


annotations : List Annotation -> String
annotations =
    List.map (\annotation -> annotation.key ++ " = " ++ annotation.value) >> String.join ", "


pairs : List KeyValuePair.KeyValuePair -> String
pairs =
    List.map (\pair -> pair.key ++ ": " ++ pair.value) >> String.join ", "


dict : Dict String String -> String
dict =
    Dict.toList >> List.map (\( key, value ) -> key ++ " = " ++ value) >> String.join ", "


yesNo : Gettext.Locale -> Bool -> String
yesNo locale value =
    if value then
        gettext "Yes" locale

    else
        gettext "No" locale


measures : Gettext.Locale -> KnowledgeModel.KnowledgeModel -> List MetricMeasure -> String
measures locale km =
    List.map
        (\measure ->
            String.format (gettext "%s (weight %s, measure %s)" locale)
                [ nameOf km measure.metricUuid
                , String.fromFloat measure.weight
                , String.fromFloat measure.measure
                ]
        )
        >> String.join ", "


validations : List QuestionValidation -> String
validations =
    List.map QuestionValidation.toOptionString >> String.join ", "


{-| An annotation set is a field like any other, and every event has one.
-}
editingAnnotations :
    Gettext.Locale
    -> (String -> String -> String -> String -> DescriptionList.Entry Msg)
    -> KnowledgeModel.KnowledgeModel
    -> Event
    -> EventField.EventField (List Annotation)
    -> List (DescriptionList.Entry Msg)
editingAnnotations locale field km event eventField =
    case EventField.getValue eventField of
        Just value ->
            [ field "annotations" (gettext "Annotations" locale) (annotations (currentAnnotations km event)) (annotations value) ]

        Nothing ->
            []


currentAnnotations : KnowledgeModel.KnowledgeModel -> Event -> List Annotation
currentAnnotations km event =
    [ Maybe.map .annotations (KnowledgeModel.getChapter event.entityUuid km)
    , Maybe.map Question.getAnnotations (KnowledgeModel.getQuestion event.entityUuid km)
    , Maybe.map .annotations (KnowledgeModel.getAnswer event.entityUuid km)
    , Maybe.map .annotations (KnowledgeModel.getChoice event.entityUuid km)
    , Maybe.map .annotations (KnowledgeModel.getMetric event.entityUuid km)
    , Maybe.map .annotations (KnowledgeModel.getPhase event.entityUuid km)
    , Maybe.map .annotations (KnowledgeModel.getTag event.entityUuid km)
    , Maybe.map .annotations (KnowledgeModel.getResourceCollection event.entityUuid km)
    , Maybe.map .annotations (KnowledgeModel.getResourcePage event.entityUuid km)
    ]
        |> Maybe.values
        |> List.head
        |> Maybe.withDefault []


{-| A reference's fields, which differ by the kind of reference it is.
-}
referenceResourcePage : Reference.Reference -> Maybe String
referenceResourcePage value =
    case value of
        Reference.ResourcePageReference data ->
            data.resourcePageUuid

        _ ->
            Nothing


referenceUrl : Reference.Reference -> Maybe String
referenceUrl value =
    case value of
        Reference.URLReference data ->
            Just data.url

        _ ->
            Nothing


referenceLabel : Reference.Reference -> Maybe String
referenceLabel value =
    case value of
        Reference.URLReference data ->
            Just data.label

        _ ->
            Nothing


referenceTarget : Reference.Reference -> Maybe String
referenceTarget value =
    case value of
        Reference.CrossReference data ->
            Just data.targetUuid

        _ ->
            Nothing


referenceDescription : Reference.Reference -> Maybe String
referenceDescription value =
    case value of
        Reference.CrossReference data ->
            Just data.description

        _ ->
            Nothing


{-| An integration's fields, which differ by whether it calls an API or a
plugin. A value read from the wrong kind is the default, which is never shown
because the event that carries that field belongs to the same kind.
-}
apiValue : Maybe Integration.Integration -> (ApiIntegrationData -> a) -> a -> a
apiValue integration get fallback =
    case integration of
        Just (Integration.ApiIntegration data) ->
            get data

        _ ->
            fallback


pluginValue : Maybe Integration.Integration -> (PluginIntegrationData -> String) -> String
pluginValue integration get =
    case integration of
        Just (Integration.PluginIntegration data) ->
            get data

        _ ->
            ""


{-| What is happening to the entity, as a word and as a class. Never a color
alone: the word is in the path for a screen reader and the class is for the eye.
-}
changeState : AppState -> Event -> ( String, String )
changeState appState event =
    case event.content of
        AddKnowledgeModelEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddChapterEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddQuestionEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddAnswerEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddChoiceEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddExpertEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddReferenceEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddMetricEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddPhaseEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddTagEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddIntegrationEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddResourceCollectionEvent _ ->
            ( "added", gettext "Added" appState.locale )

        AddResourcePageEvent _ ->
            ( "added", gettext "Added" appState.locale )

        DeleteChapterEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteQuestionEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteAnswerEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteChoiceEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteExpertEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteReferenceEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteMetricEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeletePhaseEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteTagEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteIntegrationEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteResourceCollectionEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        DeleteResourcePageEvent ->
            ( "deleted", gettext "Deleted" appState.locale )

        _ ->
            ( "changed", gettext "Changed" appState.locale )


{-| What the entity the event is about is, as a picture. An entity being added
is not in the knowledge model yet, so the event's own kind says what it will be.
-}
entityIcon : AppState -> KnowledgeModel.KnowledgeModel -> Event -> Icon.Icon
entityIcon appState km event =
    case namedEntity appState km event.entityUuid of
        Just node ->
            node.icon

        Nothing ->
            case event.content of
                AddChapterEvent _ ->
                    Icon.regular "fa-file"

                AddQuestionEvent _ ->
                    Icon.regular "fa-comment"

                AddAnswerEvent _ ->
                    Icon.regular "fa-circle-dot"

                AddChoiceEvent _ ->
                    Icon.regular "fa-square-check"

                AddExpertEvent _ ->
                    Icon.regular "fa-user"

                AddReferenceEvent _ ->
                    Icon.regular "fa-bookmark"

                AddMetricEvent _ ->
                    Icon.solid "fa-chart-column"

                AddPhaseEvent _ ->
                    Icon.regular "fa-clock"

                AddTagEvent _ ->
                    Icon.solid "fa-tag"

                AddIntegrationEvent _ ->
                    Icon.solid "fa-right-left"

                AddResourceCollectionEvent _ ->
                    Icon.solid "fa-book"

                AddResourcePageEvent _ ->
                    Icon.regular "fa-file-lines"

                _ ->
                    Icon.solid "fa-sitemap"


{-| What the entity the event is about is called. A move or a delete carries
only the uuid, and an edit only the fields it changes, so the knowledge model
being migrated says the rest.
-}
entityName : AppState -> KnowledgeModel.KnowledgeModel -> Event -> String
entityName appState km event =
    Event.getEntityVisibleName event
        |> Maybe.orElseLazy (\_ -> knownName km event.entityUuid)
        |> Maybe.filter (not << String.isEmpty)
        |> Maybe.withDefault (gettext "Untitled" appState.locale)


{-| What kind of change this is, in words. The event's own name is a
discriminator; this is the sentence a reader needs.
-}
changeKind : AppState -> Event -> String
changeKind appState event =
    case event.content of
        AddKnowledgeModelEvent _ ->
            gettext "New knowledge model" appState.locale

        EditKnowledgeModelEvent _ ->
            gettext "Knowledge model changed" appState.locale

        AddChapterEvent _ ->
            gettext "New chapter" appState.locale

        EditChapterEvent _ ->
            gettext "Chapter changed" appState.locale

        DeleteChapterEvent ->
            gettext "Chapter deleted" appState.locale

        AddMetricEvent _ ->
            gettext "New metric" appState.locale

        EditMetricEvent _ ->
            gettext "Metric changed" appState.locale

        DeleteMetricEvent ->
            gettext "Metric deleted" appState.locale

        AddPhaseEvent _ ->
            gettext "New phase" appState.locale

        EditPhaseEvent _ ->
            gettext "Phase changed" appState.locale

        DeletePhaseEvent ->
            gettext "Phase deleted" appState.locale

        AddTagEvent _ ->
            gettext "New question tag" appState.locale

        EditTagEvent _ ->
            gettext "Question tag changed" appState.locale

        DeleteTagEvent ->
            gettext "Question tag deleted" appState.locale

        AddIntegrationEvent _ ->
            gettext "New integration" appState.locale

        EditIntegrationEvent _ ->
            gettext "Integration changed" appState.locale

        DeleteIntegrationEvent ->
            gettext "Integration deleted" appState.locale

        AddQuestionEvent _ ->
            gettext "New question" appState.locale

        EditQuestionEvent _ ->
            gettext "Question changed" appState.locale

        DeleteQuestionEvent ->
            gettext "Question deleted" appState.locale

        AddAnswerEvent _ ->
            gettext "New answer" appState.locale

        EditAnswerEvent _ ->
            gettext "Answer changed" appState.locale

        DeleteAnswerEvent ->
            gettext "Answer deleted" appState.locale

        AddChoiceEvent _ ->
            gettext "New choice" appState.locale

        EditChoiceEvent _ ->
            gettext "Choice changed" appState.locale

        DeleteChoiceEvent ->
            gettext "Choice deleted" appState.locale

        AddReferenceEvent _ ->
            gettext "New reference" appState.locale

        EditReferenceEvent _ ->
            gettext "Reference changed" appState.locale

        DeleteReferenceEvent ->
            gettext "Reference deleted" appState.locale

        AddExpertEvent _ ->
            gettext "New expert" appState.locale

        EditExpertEvent _ ->
            gettext "Expert changed" appState.locale

        DeleteExpertEvent ->
            gettext "Expert deleted" appState.locale

        AddResourceCollectionEvent _ ->
            gettext "New resource collection" appState.locale

        EditResourceCollectionEvent _ ->
            gettext "Resource collection changed" appState.locale

        DeleteResourceCollectionEvent ->
            gettext "Resource collection deleted" appState.locale

        AddResourcePageEvent _ ->
            gettext "New resource page" appState.locale

        EditResourcePageEvent _ ->
            gettext "Resource page changed" appState.locale

        DeleteResourcePageEvent ->
            gettext "Resource page deleted" appState.locale

        MoveQuestionEvent _ ->
            gettext "Question moved" appState.locale

        MoveAnswerEvent _ ->
            gettext "Answer moved" appState.locale

        MoveChoiceEvent _ ->
            gettext "Choice moved" appState.locale

        MoveReferenceEvent _ ->
            gettext "Reference moved" appState.locale

        MoveExpertEvent _ ->
            gettext "Expert moved" appState.locale
