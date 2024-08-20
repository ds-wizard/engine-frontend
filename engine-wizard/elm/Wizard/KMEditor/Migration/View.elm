module Wizard.KMEditor.Migration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, br, button, code, dd, del, div, dl, dt, h1, h3, ins, label, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List.Extra as List
import Shared.Data.Event as Event exposing (Event(..))
import Shared.Data.Event.AddAnswerEventData exposing (AddAnswerEventData)
import Shared.Data.Event.AddChapterEventData exposing (AddChapterEventData)
import Shared.Data.Event.AddChoiceEventData exposing (AddChoiceEventData)
import Shared.Data.Event.AddExpertEventData exposing (AddExpertEventData)
import Shared.Data.Event.AddIntegrationEventData as AddIntegrationEventData exposing (AddIntegrationEventData(..))
import Shared.Data.Event.AddMetricEventData exposing (AddMetricEventData)
import Shared.Data.Event.AddPhaseEventData exposing (AddPhaseEventData)
import Shared.Data.Event.AddQuestionEventData as AddQuestionEventQuestionData exposing (AddQuestionEventData(..))
import Shared.Data.Event.AddReferenceCrossEventData exposing (AddReferenceCrossEventData)
import Shared.Data.Event.AddReferenceEventData as AddReferenceEventData exposing (AddReferenceEventData)
import Shared.Data.Event.AddReferenceResourcePageEventData exposing (AddReferenceResourcePageEventData)
import Shared.Data.Event.AddReferenceURLEventData exposing (AddReferenceURLEventData)
import Shared.Data.Event.AddResourceCollectionEventData exposing (AddResourceCollectionEventData)
import Shared.Data.Event.AddResourcePageEventData exposing (AddResourcePageEventData)
import Shared.Data.Event.AddTagEventData exposing (AddTagEventData)
import Shared.Data.Event.EditAnswerEventData exposing (EditAnswerEventData)
import Shared.Data.Event.EditChapterEventData exposing (EditChapterEventData)
import Shared.Data.Event.EditChoiceEventData exposing (EditChoiceEventData)
import Shared.Data.Event.EditExpertEventData exposing (EditExpertEventData)
import Shared.Data.Event.EditIntegrationEventData as EditIntegrationEventData exposing (EditIntegrationEventData(..))
import Shared.Data.Event.EditKnowledgeModelEventData exposing (EditKnowledgeModelEventData)
import Shared.Data.Event.EditMetricEventData exposing (EditMetricEventData)
import Shared.Data.Event.EditPhaseEventData exposing (EditPhaseEventData)
import Shared.Data.Event.EditQuestionEventData as EditQuestionEventData exposing (EditQuestionEventData(..))
import Shared.Data.Event.EditReferenceCrossEventData exposing (EditReferenceCrossEventData)
import Shared.Data.Event.EditReferenceEventData as EditReferenceEventData exposing (EditReferenceEventData(..))
import Shared.Data.Event.EditReferenceResourcePageEventData exposing (EditReferenceResourcePageEventData)
import Shared.Data.Event.EditReferenceURLEventData exposing (EditReferenceURLEventData)
import Shared.Data.Event.EditResourceCollectionEventData exposing (EditResourceCollectionEventData)
import Shared.Data.Event.EditResourcePageEventData exposing (EditResourcePageEventData)
import Shared.Data.Event.EditTagEventData exposing (EditTagEventData)
import Shared.Data.Event.EventField as EventField
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration as Integration exposing (Integration(..))
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Shared.Data.KnowledgeModel.Reference.CrossReferenceData exposing (CrossReferenceData)
import Shared.Data.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import Shared.Data.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)
import Shared.Data.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Shared.Data.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Data.Migration exposing (Migration)
import Shared.Data.Migration.MigrationState.MigrationStateType exposing (MigrationStateType(..))
import Shared.Html exposing (emptyNode, faSet)
import String.Format as String exposing (format)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Migration.Models exposing (ButtonClicked(..), Model)
import Wizard.KMEditor.Migration.Msgs exposing (Msg(..))
import Wizard.KMEditor.Migration.View.DiffTree as DiffTree
import Wizard.Routes as Routes


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (migrationView appState model) model.migration


migrationView : AppState -> Model -> Migration -> Html Msg
migrationView appState model migration =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text (gettext "Migration state is corrupted." appState.locale) ]

        currentView =
            case migration.migrationState.stateType of
                ConflictState ->
                    let
                        conflictView =
                            migration.migrationState.targetEvent
                                |> Maybe.map (getEventView appState model migration)
                                |> Maybe.map (List.singleton >> div [ class "col-8" ])
                                |> Maybe.withDefault (div [ class "col-12" ] [ errorMessage ])

                        kmName =
                            ActionResult.unwrap "" .branchName model.migration

                        diffTree =
                            migration.migrationState.targetEvent
                                |> Maybe.map (DiffTree.view appState kmName migration.currentKnowledgeModel)
                                |> Maybe.map (List.singleton >> div [ class "col-4" ])
                                |> Maybe.withDefault emptyNode
                    in
                    div [ class "row" ]
                        [ migrationSummary appState migration, conflictView, diffTree ]

                CompletedState ->
                    viewCompletedMigration appState model

                RunningState ->
                    div [ class "alert alert-warning" ]
                        [ text (gettext "Migration is still running, try again later." appState.locale) ]

                _ ->
                    errorMessage
    in
    div [ class "col KMEditor__Migration", dataCy "km-editor_migration" ]
        [ div [] [ Page.header (gettext "Migration" appState.locale) [] ]
        , FormResult.view appState model.conflict
        , currentView
        ]


migrationSummary : AppState -> Migration -> Html Msg
migrationSummary appState migration =
    div [ class "col-12" ]
        [ p []
            (String.formatHtml (gettext "Migration of %s from %s to %s." appState.locale)
                [ strong [] [ text migration.branchName ]
                , code [] [ text migration.branchPreviousPackageId ]
                , code [] [ text migration.targetPackageId ]
                ]
            )
        ]


getEventView : AppState -> Model -> Migration -> Event -> Html Msg
getEventView appState model migration event =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text (gettext "The event is not connected to any entity in the Knowledge Model." appState.locale) ]
    in
    case event of
        AddKnowledgeModelEvent _ _ ->
            -- AddKnowledgeModelEvent should never appear in migrations
            emptyNode

        EditKnowledgeModelEvent eventData _ ->
            migration.currentKnowledgeModel
                |> viewEditKnowledgeModelDiff appState eventData
                |> viewEvent appState model event (gettext "Edit Knowledge Model" appState.locale)

        AddMetricEvent eventData _ ->
            viewAddMetricDiff appState eventData
                |> viewEvent appState model event (gettext "Add metric" appState.locale)

        EditMetricEvent eventData commonData ->
            KnowledgeModel.getMetric commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditMetricDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit metric" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteMetricEvent commonData ->
            KnowledgeModel.getMetric commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteMetricDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete metric" appState.locale))
                |> Maybe.withDefault errorMessage

        AddPhaseEvent eventData _ ->
            viewAddPhaseDiff appState eventData
                |> viewEvent appState model event (gettext "Add phase" appState.locale)

        EditPhaseEvent eventData commonData ->
            KnowledgeModel.getPhase commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditPhaseDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit phase" appState.locale))
                |> Maybe.withDefault errorMessage

        DeletePhaseEvent commonData ->
            KnowledgeModel.getPhase commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeletePhaseDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete phase" appState.locale))
                |> Maybe.withDefault errorMessage

        AddTagEvent eventData _ ->
            viewAddTagDiff appState eventData
                |> viewEvent appState model event (gettext "Add question tag" appState.locale)

        EditTagEvent eventData commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditTagDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit question tag" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteTagEvent commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteTagDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete question tag" appState.locale))
                |> Maybe.withDefault errorMessage

        AddIntegrationEvent eventData _ ->
            viewAddIntegrationDiff appState eventData
                |> viewEvent appState model event (gettext "Add integration" appState.locale)

        EditIntegrationEvent eventData commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditIntegrationDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit integration" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteIntegrationEvent commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteIntegrationDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete integration" appState.locale))
                |> Maybe.withDefault errorMessage

        AddChapterEvent eventData _ ->
            viewAddChapterDiff appState eventData
                |> viewEvent appState model event (gettext "Add chapter" appState.locale)

        EditChapterEvent eventData commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditChapterDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit chapter" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteChapterEvent commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteChapterDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (gettext "Delete chapter" appState.locale))
                |> Maybe.withDefault errorMessage

        AddQuestionEvent eventData _ ->
            viewAddQuestionDiff appState migration.currentKnowledgeModel eventData
                |> viewEvent appState model event (gettext "Add question" appState.locale)

        EditQuestionEvent eventData commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditQuestionDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit question" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteQuestionEvent commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteQuestionDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (gettext "Delete question" appState.locale))
                |> Maybe.withDefault errorMessage

        AddAnswerEvent eventData _ ->
            viewAddAnswerDiff appState migration.currentKnowledgeModel eventData
                |> viewEvent appState model event (gettext "Add answer" appState.locale)

        EditAnswerEvent eventData commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditAnswerDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit answer" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteAnswerEvent commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteAnswerDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (gettext "Delete answer" appState.locale))
                |> Maybe.withDefault errorMessage

        AddChoiceEvent eventData _ ->
            viewAddChoiceDiff appState eventData
                |> viewEvent appState model event (gettext "Add choice" appState.locale)

        EditChoiceEvent eventData commonData ->
            KnowledgeModel.getChoice commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditChoiceDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit choice" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteChoiceEvent commonData ->
            KnowledgeModel.getChoice commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteChoiceDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete choice" appState.locale))
                |> Maybe.withDefault errorMessage

        AddReferenceEvent eventData _ ->
            viewAddReferenceDiff appState eventData
                |> viewEvent appState model event (gettext "Add reference" appState.locale)

        EditReferenceEvent eventData commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditReferenceDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit reference" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteReferenceEvent commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteReferenceDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete reference" appState.locale))
                |> Maybe.withDefault errorMessage

        AddExpertEvent eventData _ ->
            viewAddExpertDiff appState eventData
                |> viewEvent appState model event (gettext "Add expert" appState.locale)

        EditExpertEvent eventData commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditExpertDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit expert" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteExpertEvent commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteExpertDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete expert" appState.locale))
                |> Maybe.withDefault errorMessage

        AddResourceCollectionEvent eventData _ ->
            viewAddResourceCollectionDiff appState eventData
                |> viewEvent appState model event (gettext "Add resource collection" appState.locale)

        EditResourceCollectionEvent eventData commonData ->
            KnowledgeModel.getResourceCollection commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditResourceCollectionDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit resource collection" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteResourceCollectionEvent commonData ->
            KnowledgeModel.getResourceCollection commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteResourceCollectionDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (gettext "Delete resource collection" appState.locale))
                |> Maybe.withDefault errorMessage

        AddResourcePageEvent eventData _ ->
            viewAddResourcePageDiff appState eventData
                |> viewEvent appState model event (gettext "Add resource page" appState.locale)

        EditResourcePageEvent eventData commonData ->
            KnowledgeModel.getResourcePage commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditResourcePageDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (gettext "Edit resource page" appState.locale))
                |> Maybe.withDefault errorMessage

        DeleteResourcePageEvent commonData ->
            KnowledgeModel.getResourcePage commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteResourcePageDiff appState)
                |> Maybe.map (viewEvent appState model event (gettext "Delete resource page" appState.locale))
                |> Maybe.withDefault errorMessage

        MoveQuestionEvent _ commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveQuestion appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (gettext "Move question" appState.locale))
                |> Maybe.withDefault errorMessage

        MoveAnswerEvent _ commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveAnswer appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (gettext "Move answer" appState.locale))
                |> Maybe.withDefault errorMessage

        MoveChoiceEvent _ commonData ->
            KnowledgeModel.getChoice commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveChoice appState)
                |> Maybe.map (viewEvent appState model event (gettext "Move choice" appState.locale))
                |> Maybe.withDefault errorMessage

        MoveReferenceEvent _ commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveReference appState)
                |> Maybe.map (viewEvent appState model event (gettext "Move reference" appState.locale))
                |> Maybe.withDefault errorMessage

        MoveExpertEvent _ commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveExpert appState)
                |> Maybe.map (viewEvent appState model event (gettext "Move expert" appState.locale))
                |> Maybe.withDefault errorMessage


viewEvent : AppState -> Model -> Event -> String -> Html Msg -> Html Msg
viewEvent appState model event name diffView =
    div [ dataCy ("km-migration_event_" ++ Event.getUuid event) ]
        [ h3 [] [ text name ]
        , div [ class "card bg-light" ]
            [ div [ class "card-body" ]
                [ formActions appState model
                , diffView
                ]
            ]
        ]


viewEditKnowledgeModelDiff : AppState -> EditKnowledgeModelEventData -> KnowledgeModel -> Html Msg
viewEditKnowledgeModelDiff appState event km =
    let
        chapters =
            KnowledgeModel.getChapters km

        originalChapters =
            List.map .uuid chapters

        chapterNames =
            Dict.fromList <| List.map (\c -> ( c.uuid, c.title )) chapters

        chaptersDiff =
            viewDiffChildren (gettext "Chapters" appState.locale) originalChapters (EventField.getValueWithDefault event.chapterUuids originalChapters) chapterNames

        metrics =
            KnowledgeModel.getMetrics km

        originalMetrics =
            List.map .uuid metrics

        metricNames =
            Dict.fromList <| List.map (\m -> ( m.uuid, m.title )) metrics

        metricsDiff =
            viewDiffChildren (gettext "Metrics" appState.locale)
                originalMetrics
                (EventField.getValueWithDefault event.metricUuids originalMetrics)
                metricNames

        phases =
            KnowledgeModel.getPhases km

        originalPhases =
            List.map .uuid phases

        phaseNames =
            Dict.fromList <| List.map (\p -> ( p.uuid, p.title )) phases

        phasesDiff =
            viewDiffChildren (gettext "Phases" appState.locale)
                originalPhases
                (EventField.getValueWithDefault event.phaseUuids originalPhases)
                phaseNames

        tags =
            KnowledgeModel.getTags km

        originalTags =
            List.map .uuid tags

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        tagsDiff =
            viewDiffChildren (gettext "Question Tags" appState.locale)
                originalTags
                (EventField.getValueWithDefault event.tagUuids originalTags)
                tagNames

        integrations =
            KnowledgeModel.getIntegrations km

        originalIntegrations =
            List.map Integration.getUuid integrations

        integrationNames =
            Dict.fromList <| List.map (\i -> ( Integration.getUuid i, Integration.getName i )) integrations

        integrationsDiff =
            viewDiffChildren (gettext "Integrations" appState.locale)
                originalIntegrations
                (EventField.getValueWithDefault event.integrationUuids originalIntegrations)
                integrationNames

        annotationsDiff =
            viewAnnotationsDiff appState km.annotations (EventField.getValueWithDefault event.annotations km.annotations)
    in
    div []
        [ chaptersDiff, metricsDiff, phasesDiff, tagsDiff, integrationsDiff, annotationsDiff ]


viewAddMetricDiff : AppState -> AddMetricEventData -> Html Msg
viewAddMetricDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Abbreviation" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ event.title
                    , Maybe.withDefault "" event.abbreviation
                    , Maybe.withDefault "" event.description
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditMetricDiff : AppState -> EditMetricEventData -> Metric -> Html Msg
viewEditMetricDiff appState event metric =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Title" appState.locale
                    , gettext "Abbreviation" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ metric.title
                    , Maybe.withDefault "" metric.abbreviation
                    , Maybe.withDefault "" metric.description
                    ]
                    [ EventField.getValueWithDefault event.title metric.title
                    , EventField.getValueWithDefault event.abbreviation metric.abbreviation |> Maybe.withDefault ""
                    , EventField.getValueWithDefault event.description metric.description |> Maybe.withDefault ""
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState metric.annotations (EventField.getValueWithDefault event.annotations metric.annotations)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteMetricDiff : AppState -> Metric -> Html Msg
viewDeleteMetricDiff appState metric =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Abbreviation" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ metric.title
                    , Maybe.withDefault "" metric.abbreviation
                    , Maybe.withDefault "" metric.description
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState metric.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddPhaseDiff : AppState -> AddPhaseEventData -> Html Msg
viewAddPhaseDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ event.title
                    , Maybe.withDefault "" event.description
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditPhaseDiff : AppState -> EditPhaseEventData -> Phase -> Html Msg
viewEditPhaseDiff appState event phase =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Title" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ phase.title
                    , Maybe.withDefault "" phase.description
                    ]
                    [ EventField.getValueWithDefault event.title phase.title
                    , EventField.getValueWithDefault event.description phase.description |> Maybe.withDefault ""
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState phase.annotations (EventField.getValueWithDefault event.annotations phase.annotations)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeletePhaseDiff : AppState -> Phase -> Html Msg
viewDeletePhaseDiff appState phase =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ phase.title
                    , Maybe.withDefault "" phase.description
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState phase.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddTagDiff : AppState -> AddTagEventData -> Html Msg
viewAddTagDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Name" appState.locale
                    , gettext "Description" appState.locale
                    , gettext "Color" appState.locale
                    ]
                    [ event.name
                    , event.description |> Maybe.withDefault ""
                    , event.color
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div []
        (fieldDiff ++ [ annotationsDiff ])


viewEditTagDiff : AppState -> EditTagEventData -> Tag -> Html Msg
viewEditTagDiff appState event tag =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Name" appState.locale
                    , gettext "Description" appState.locale
                    , gettext "Color" appState.locale
                    ]
                    [ tag.name
                    , Maybe.withDefault "" tag.description
                    , tag.color
                    ]
                    [ EventField.getValueWithDefault event.name tag.name
                    , EventField.getValueWithDefault event.description tag.description |> Maybe.withDefault ""
                    , EventField.getValueWithDefault event.color tag.color
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState tag.annotations (EventField.getValueWithDefault event.annotations tag.annotations)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteTagDiff : AppState -> Tag -> Html Msg
viewDeleteTagDiff appState tag =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Name" appState.locale
                    , gettext "Description" appState.locale
                    , gettext "Color" appState.locale
                    ]
                    [ tag.name
                    , tag.description |> Maybe.withDefault ""
                    , tag.color
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState tag.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddIntegrationDiff : AppState -> AddIntegrationEventData -> Html Msg
viewAddIntegrationDiff appState event =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ gettext "Type" appState.locale
                , gettext "ID" appState.locale
                , gettext "Name" appState.locale
                , gettext "Props" appState.locale
                , gettext "Item URL" appState.locale
                ]
                [ AddIntegrationEventData.getTypeString event
                , AddIntegrationEventData.map .id .id event
                , AddIntegrationEventData.map .name .name event
                , String.join ", " <| AddIntegrationEventData.map .props .props event
                , Maybe.withDefault "" <| AddIntegrationEventData.map .itemUrl .itemUrl event
                ]

        extraFields =
            case event of
                AddIntegrationApiEvent data ->
                    List.map2 (\a b -> ( a, b ))
                        [ gettext "Request HTTP Method" appState.locale
                        , gettext "Request URL" appState.locale
                        , gettext "Request HTTP Headers" appState.locale
                        , gettext "Request HTTP Body" appState.locale
                        , gettext "Response List Field" appState.locale
                        , gettext "Response Item ID" appState.locale
                        , gettext "Response Item Template" appState.locale
                        ]
                        [ data.requestMethod
                        , data.requestUrl
                        , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) data.requestHeaders
                        , data.requestBody
                        , Maybe.withDefault "" data.responseListField
                        , Maybe.withDefault "" data.responseItemId
                        , data.responseItemTemplate
                        ]

                AddIntegrationWidgetEvent data ->
                    List.map2 (\a b -> ( a, b ))
                        [ gettext "Widget URL" appState.locale
                        ]
                        [ data.widgetUrl
                        ]

        fieldDiff =
            viewAdd (fields ++ extraFields)

        annotationsDiff =
            viewAnnotationsDiff appState [] (AddIntegrationEventData.map .annotations .annotations event)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditIntegrationDiff : AppState -> EditIntegrationEventData -> Integration -> Html Msg
viewEditIntegrationDiff appState event integration =
    let
        fields =
            List.map3 (\a b c -> ( a, b, c ))
                [ gettext "Type" appState.locale
                , gettext "ID" appState.locale
                , gettext "Name" appState.locale
                , gettext "Props" appState.locale
                , gettext "Item URL" appState.locale
                ]
                [ Integration.getTypeString integration
                , Integration.getId integration
                , Integration.getName integration
                , String.join ", " <| Integration.getProps integration
                , Maybe.withDefault "" <| Integration.getItemUrl integration
                ]
                [ EditIntegrationEventData.getTypeString event
                , EventField.getValueWithDefault (EditIntegrationEventData.map .id .id event) (Integration.getId integration)
                , EventField.getValueWithDefault (EditIntegrationEventData.map .name .name event) (Integration.getName integration)
                , String.join ", " <| EventField.getValueWithDefault (EditIntegrationEventData.map .props .props event) (Integration.getProps integration)
                , Maybe.withDefault "" <| EventField.getValueWithDefault (EditIntegrationEventData.map .itemUrl .itemUrl event) (Integration.getItemUrl integration)
                ]

        extraFields =
            case event of
                EditIntegrationApiEvent data ->
                    List.map3 (\a b c -> ( a, b, c ))
                        [ gettext "Request HTTP Method" appState.locale
                        , gettext "Request URL" appState.locale
                        , gettext "Request HTTP Headers" appState.locale
                        , gettext "Request HTTP Body" appState.locale
                        , gettext "Response List Field" appState.locale
                        , gettext "Response Item ID" appState.locale
                        , gettext "Response Item Template" appState.locale
                        ]
                        [ Maybe.withDefault "" <| Integration.getRequestMethod integration
                        , Maybe.withDefault "" <| Integration.getRequestUrl integration
                        , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) <| Maybe.withDefault [] <| Integration.getRequestHeaders integration
                        , Maybe.withDefault "" <| Integration.getRequestBody integration
                        , Maybe.withDefault "" <| Integration.getResponseListField integration
                        , Maybe.withDefault "" <| Integration.getResponseItemId integration
                        , Maybe.withDefault "" <| Integration.getResponseItemId integration
                        ]
                        [ EventField.getValueWithDefault data.requestMethod (Maybe.withDefault "" <| Integration.getRequestMethod integration)
                        , EventField.getValueWithDefault data.requestUrl (Maybe.withDefault "" <| Integration.getRequestUrl integration)
                        , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) <| EventField.getValueWithDefault data.requestHeaders (Maybe.withDefault [] <| Integration.getRequestHeaders integration)
                        , EventField.getValueWithDefault data.requestBody (Maybe.withDefault "" <| Integration.getRequestBody integration)
                        , Maybe.withDefault "" <| EventField.getValueWithDefault data.responseListField (Integration.getResponseListField integration)
                        , Maybe.withDefault "" <| EventField.getValueWithDefault data.responseItemId (Integration.getResponseItemId integration)
                        , EventField.getValueWithDefault data.responseItemTemplate (Maybe.withDefault "" <| Integration.getResponseItemTemplate integration)
                        ]

                EditIntegrationWidgetEvent data ->
                    List.map3 (\a b c -> ( a, b, c ))
                        [ gettext "Widget URL" appState.locale
                        ]
                        [ Maybe.withDefault "" <| Integration.getWidgetUrl integration
                        ]
                        [ EventField.getValueWithDefault data.widgetUrl (Maybe.withDefault "" <| Integration.getWidgetUrl integration)
                        ]

        fieldDiff =
            viewDiff (fields ++ extraFields)

        annotationsDiff =
            viewAnnotationsDiff appState
                (Integration.getAnnotations integration)
                (EventField.getValueWithDefault (EditIntegrationEventData.map .annotations .annotations event) (Integration.getAnnotations integration))
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteIntegrationDiff : AppState -> Integration -> Html Msg
viewDeleteIntegrationDiff appState integration =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ gettext "Type" appState.locale
                , gettext "ID" appState.locale
                , gettext "Name" appState.locale
                , gettext "Props" appState.locale
                , gettext "Item URL" appState.locale
                ]
                [ Integration.getTypeString integration
                , Integration.getId integration
                , Integration.getName integration
                , String.join ", " <| Integration.getProps integration
                , Maybe.withDefault "" <| Integration.getItemUrl integration
                ]

        extraFields =
            case integration of
                ApiIntegration _ data ->
                    List.map2 (\a b -> ( a, b ))
                        [ gettext "Request HTTP Method" appState.locale
                        , gettext "Request URL" appState.locale
                        , gettext "Request HTTP Headers" appState.locale
                        , gettext "Request HTTP Body" appState.locale
                        , gettext "Response List Field" appState.locale
                        , gettext "Response Item ID" appState.locale
                        , gettext "Response Item Template" appState.locale
                        ]
                        [ data.requestMethod
                        , data.requestUrl
                        , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) data.requestHeaders
                        , data.requestBody
                        , Maybe.withDefault "" <| data.responseListField
                        , Maybe.withDefault "" <| data.responseItemId
                        , data.responseItemTemplate
                        ]

                WidgetIntegration _ data ->
                    List.map2 (\a b -> ( a, b ))
                        [ gettext "Widget URL" appState.locale
                        ]
                        [ data.widgetUrl
                        ]

        fieldDiff =
            viewDelete (fields ++ extraFields)

        annotationsDiff =
            viewAnnotationsDiff appState (Integration.getAnnotations integration) []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddChapterDiff : AppState -> AddChapterEventData -> Html Msg
viewAddChapterDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Text" appState.locale
                    ]
                    [ event.title
                    , Maybe.withDefault "" event.text
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditChapterDiff : AppState -> KnowledgeModel -> EditChapterEventData -> Chapter -> Html Msg
viewEditChapterDiff appState km event chapter =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Title" appState.locale
                    , gettext "Text" appState.locale
                    ]
                    [ chapter.title
                    , Maybe.withDefault "" chapter.text
                    ]
                    [ EventField.getValueWithDefault event.title chapter.title
                    , EventField.getValueWithDefault event.text chapter.text |> Maybe.withDefault ""
                    ]

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km

        originalQuestions =
            List.map Question.getUuid questions

        questionNames =
            Dict.fromList <| List.map (\q -> ( Question.getUuid q, Question.getTitle q )) questions

        questionsDiff =
            viewDiffChildren (gettext "Questions" appState.locale)
                originalQuestions
                (EventField.getValueWithDefault event.questionUuids originalQuestions)
                questionNames

        annotationsDiff =
            viewAnnotationsDiff appState chapter.annotations (EventField.getValueWithDefault event.annotations chapter.annotations)
    in
    div [] (fieldDiff ++ [ questionsDiff, annotationsDiff ])


viewDeleteChapterDiff : AppState -> KnowledgeModel -> Chapter -> Html Msg
viewDeleteChapterDiff appState km chapter =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Text" appState.locale
                    ]
                    [ chapter.title
                    , Maybe.withDefault "" chapter.text
                    ]

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km

        questionNames =
            List.map Question.getTitle questions

        questionsDiff =
            viewDeletedChildren (gettext "Questions" appState.locale) questionNames

        annotationsDiff =
            viewAnnotationsDiff appState chapter.annotations []
    in
    div [] (fieldDiff ++ [ questionsDiff, annotationsDiff ])


viewAddQuestionDiff : AppState -> KnowledgeModel -> AddQuestionEventData -> Html Msg
viewAddQuestionDiff appState km event =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ gettext "Type" appState.locale
                , gettext "Title" appState.locale
                , gettext "Text" appState.locale
                ]
                [ AddQuestionEventQuestionData.getTypeString event
                , AddQuestionEventQuestionData.map .title .title .title .title .title event
                , AddQuestionEventQuestionData.map .text .text .text .text .text event |> Maybe.withDefault ""
                ]

        extraFields =
            case event of
                AddQuestionValueEvent data ->
                    [ ( gettext "Value Type" appState.locale, QuestionValueType.toString data.valueType ) ]

                AddQuestionIntegrationEvent data ->
                    [ ( gettext "Integration" appState.locale, getIntegrationName km data.integrationUuid ) ]

                _ ->
                    []

        fieldsDiff =
            viewAdd (fields ++ extraFields)

        integrationPropsDiff =
            case event of
                AddQuestionIntegrationEvent data ->
                    let
                        props =
                            List.map (\( p, v ) -> p ++ " = " ++ v) <| Dict.toList data.props
                    in
                    viewAddedChildren (gettext "Props" appState.locale) props

                _ ->
                    emptyNode

        tags =
            KnowledgeModel.getTags km

        tagUuids =
            AddQuestionEventQuestionData.map .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids event

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        originalTags =
            List.map (\_ -> "") tagUuids

        tagsDiff =
            viewDiffChildren (gettext "Question Tags" appState.locale) originalTags tagUuids tagNames

        annotations =
            AddQuestionEventQuestionData.map .annotations .annotations .annotations .annotations .annotations event

        annotationsDiff =
            viewAnnotationsDiff appState [] annotations
    in
    div []
        (fieldsDiff ++ [ integrationPropsDiff, tagsDiff, annotationsDiff ])


viewEditQuestionDiff : AppState -> KnowledgeModel -> EditQuestionEventData -> Question -> Html Msg
viewEditQuestionDiff appState km event question =
    let
        -- Fields
        questionUuid =
            Question.getUuid question

        title =
            EditQuestionEventData.map .title .title .title .title .title event

        questionText =
            EditQuestionEventData.map .text .text .text .text .text event

        fields =
            List.map3 (\a b c -> ( a, b, c ))
                [ gettext "Type" appState.locale
                , gettext "Title" appState.locale
                , gettext "Text" appState.locale
                ]
                [ Question.getTypeString question
                , Question.getTitle question
                , Question.getText question |> Maybe.withDefault ""
                ]
                [ EditQuestionEventData.getTypeString event
                , EventField.getValueWithDefault title <| Question.getTitle question
                , EventField.getValueWithDefault questionText (Question.getText question) |> Maybe.withDefault ""
                ]

        extraFields =
            case event of
                EditQuestionValueEvent data ->
                    case ( Question.getValueType question, EventField.getValue data.valueType ) of
                        ( Nothing, Nothing ) ->
                            []

                        ( original, new ) ->
                            let
                                originalStr =
                                    Maybe.withDefault "" <| Maybe.map QuestionValueType.toString original

                                newStr =
                                    Maybe.withDefault originalStr <| Maybe.map QuestionValueType.toString new
                            in
                            [ ( gettext "Value Type" appState.locale, originalStr, newStr ) ]

                EditQuestionIntegrationEvent data ->
                    case ( Question.getIntegrationUuid question, EventField.getValue data.integrationUuid ) of
                        ( Nothing, Nothing ) ->
                            []

                        ( originalIntegrationUuid, newIntegrationUuid ) ->
                            let
                                originalStr =
                                    originalIntegrationUuid
                                        |> Maybe.map (getIntegrationName km)
                                        |> Maybe.withDefault ""

                                newStr =
                                    newIntegrationUuid
                                        |> Maybe.map (getIntegrationName km)
                                        |> Maybe.withDefault originalStr
                            in
                            [ ( gettext "Integration" appState.locale, originalStr, newStr ) ]

                _ ->
                    []

        fieldDiff =
            viewDiff (fields ++ extraFields)

        -- Integration props
        integrationPropsDiff =
            case event of
                EditQuestionIntegrationEvent data ->
                    let
                        originalProps =
                            Question.getProps question
                                |> Maybe.map (List.map (\( p, v ) -> p ++ " = " ++ v) << Dict.toList)
                                |> Maybe.withDefault []

                        newProps =
                            EventField.getValue data.props
                                |> Maybe.map (List.map (\( p, v ) -> p ++ " = " ++ v) << Dict.toList)
                                |> Maybe.withDefault originalProps
                    in
                    viewAddedAndDeletedChildren (gettext "Props" appState.locale) originalProps newProps

                _ ->
                    emptyNode

        -- Tags
        tags =
            KnowledgeModel.getTags km

        originalTags =
            Question.getTagUuids question

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        tagsDiff =
            viewDiffChildren (gettext "Question Tags" appState.locale)
                originalTags
                (EventField.getValueWithDefault (EditQuestionEventData.map .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids event) originalTags)
                tagNames

        -- Answers
        answersDiff =
            case event of
                EditQuestionOptionsEvent data ->
                    let
                        answers =
                            KnowledgeModel.getQuestionAnswers questionUuid km

                        originalAnswers =
                            List.map .uuid answers

                        answerNames =
                            Dict.fromList <| List.map (\a -> ( a.uuid, a.label )) answers
                    in
                    viewDiffChildren (gettext "Answers" appState.locale)
                        originalAnswers
                        (EventField.getValueWithDefault data.answerUuids originalAnswers)
                        answerNames

                _ ->
                    emptyNode

        -- Choices
        choicesDiff =
            case question of
                MultiChoiceQuestion _ _ ->
                    viewPlainChildren (gettext "Choices" appState.locale) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionChoices questionUuid km

                _ ->
                    emptyNode

        -- Item Template Questions
        itemTemplateQuestionsDiff =
            case event of
                EditQuestionListEvent data ->
                    let
                        itemTemplateQuestions =
                            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km

                        originalItemTemplateQuestions =
                            List.map Question.getUuid itemTemplateQuestions

                        itemTemplateQuestionNames =
                            Dict.fromList <| List.map (\q -> ( Question.getUuid q, Question.getTitle q )) itemTemplateQuestions
                    in
                    viewDiffChildren (gettext "Questions" appState.locale)
                        originalItemTemplateQuestions
                        (EventField.getValueWithDefault data.itemTemplateQuestionUuids originalItemTemplateQuestions)
                        itemTemplateQuestionNames

                _ ->
                    emptyNode

        -- References
        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        originalReferences =
            List.map Reference.getUuid references

        referenceNames =
            Dict.fromList <| List.map (\r -> ( Reference.getUuid r, Reference.getVisibleName (KnowledgeModel.getAllResourcePages km) r )) references

        referencesDiff =
            viewDiffChildren (gettext "References" appState.locale)
                originalReferences
                (EventField.getValueWithDefault (EditQuestionEventData.map .referenceUuids .referenceUuids .referenceUuids .referenceUuids .referenceUuids event) originalReferences)
                referenceNames

        -- Experts
        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        originalExperts =
            List.map .uuid experts

        expertNames =
            Dict.fromList <| List.map (\e -> ( e.uuid, e.name )) experts

        expertsDiff =
            viewDiffChildren (gettext "Experts" appState.locale)
                originalExperts
                (EventField.getValueWithDefault (EditQuestionEventData.map .expertUuids .expertUuids .expertUuids .expertUuids .expertUuids event) originalExperts)
                expertNames

        -- Annotations
        originalAnnotations =
            Question.getAnnotations question

        annotations =
            EventField.getValueWithDefault
                (EditQuestionEventData.map .annotations .annotations .annotations .annotations .annotations event)
                originalAnnotations

        annotationsDiff =
            viewAnnotationsDiff appState originalAnnotations annotations
    in
    div []
        (fieldDiff ++ [ integrationPropsDiff, tagsDiff, answersDiff, choicesDiff, itemTemplateQuestionsDiff, referencesDiff, expertsDiff, annotationsDiff ])


viewDeleteQuestionDiff : AppState -> KnowledgeModel -> Question -> Html Msg
viewDeleteQuestionDiff appState km question =
    let
        -- Fields
        questionUuid =
            Question.getUuid question

        fields =
            List.map2 (\a b -> ( a, b ))
                [ gettext "Type" appState.locale
                , gettext "Title" appState.locale
                , gettext "Text" appState.locale
                ]
                [ Question.getTypeString question
                , Question.getTitle question
                , Question.getText question |> Maybe.withDefault ""
                ]

        extraFields =
            case question of
                ValueQuestion _ data ->
                    [ ( gettext "Value Type" appState.locale, QuestionValueType.toString data.valueType ) ]

                IntegrationQuestion _ data ->
                    [ ( gettext "Integration" appState.locale, getIntegrationName km data.integrationUuid ) ]

                _ ->
                    []

        fieldDiff =
            viewDelete (fields ++ extraFields)

        -- Tags
        tags =
            KnowledgeModel.getTags km

        tagNames =
            List.map .name <| List.filter (\t -> List.member t.uuid (Question.getTagUuids question)) tags

        tagsDiff =
            viewDeletedChildren (gettext "Question Tags" appState.locale) tagNames

        -- Answers
        answersDiff =
            case question of
                OptionsQuestion _ _ ->
                    viewDeletedChildren (gettext "Answers" appState.locale) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionAnswers questionUuid km

                _ ->
                    emptyNode

        -- Choices
        choicesDiff =
            case question of
                MultiChoiceQuestion _ _ ->
                    viewPlainChildren (gettext "Choices" appState.locale) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionChoices questionUuid km

                _ ->
                    emptyNode

        -- Item Template Questions
        itemTemplateQuestionsDiff =
            case question of
                ListQuestion _ _ ->
                    viewDeletedChildren (gettext "Questions" appState.locale) <|
                        List.map Question.getTitle <|
                            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km

                _ ->
                    emptyNode

        -- References
        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        referencesDiff =
            viewDeletedChildren (gettext "References" appState.locale) <| List.map (Reference.getVisibleName (KnowledgeModel.getAllResourcePages km)) references

        -- Experts
        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        expertsDiff =
            viewDeletedChildren (gettext "Experts" appState.locale) <| List.map .name experts

        -- Annotations
        annotationsDiff =
            viewAnnotationsDiff appState (Question.getAnnotations question) []
    in
    div []
        (fieldDiff ++ [ tagsDiff, answersDiff, choicesDiff, itemTemplateQuestionsDiff, referencesDiff, expertsDiff, annotationsDiff ])


viewMoveQuestion : AppState -> KnowledgeModel -> Question -> Html Msg
viewMoveQuestion appState km question =
    let
        -- Fields
        questionUuid =
            Question.getUuid question

        fields =
            List.map2 (\a b -> ( a, b ))
                [ gettext "Type" appState.locale
                , gettext "Title" appState.locale
                , gettext "Text" appState.locale
                ]
                [ Question.getTypeString question
                , Question.getTitle question
                , Question.getText question |> Maybe.withDefault ""
                ]

        extraFields =
            case question of
                ValueQuestion _ data ->
                    [ ( gettext "Value Type" appState.locale, QuestionValueType.toString data.valueType ) ]

                IntegrationQuestion _ data ->
                    [ ( gettext "Integration" appState.locale, getIntegrationName km data.integrationUuid ) ]

                _ ->
                    []

        fieldDiff =
            viewPlain (fields ++ extraFields)

        -- Tags
        tags =
            KnowledgeModel.getTags km

        tagNames =
            List.map .name <| List.filter (\t -> List.member t.uuid (Question.getTagUuids question)) tags

        tagsDiff =
            viewPlainChildren (gettext "Question Tags" appState.locale) tagNames

        -- Answers
        answersDiff =
            case question of
                OptionsQuestion _ _ ->
                    viewPlainChildren (gettext "Answers" appState.locale) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionAnswers questionUuid km

                _ ->
                    emptyNode

        -- Choices
        choicesDiff =
            case question of
                MultiChoiceQuestion _ _ ->
                    viewPlainChildren (gettext "Choices" appState.locale) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionChoices questionUuid km

                _ ->
                    emptyNode

        -- Item Template Questions
        itemTemplateQuestionsDiff =
            case question of
                ListQuestion _ _ ->
                    viewPlainChildren (gettext "Questions" appState.locale) <|
                        List.map Question.getTitle <|
                            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km

                _ ->
                    emptyNode

        -- References
        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        referencesDiff =
            viewPlainChildren (gettext "References" appState.locale) <| List.map (Reference.getVisibleName (KnowledgeModel.getAllResourcePages km)) references

        -- Experts
        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        expertsDiff =
            viewPlainChildren (gettext "Experts" appState.locale) <| List.map .name experts

        -- Annotations
        annotations =
            Question.getAnnotations question

        annotationsDiff =
            viewAnnotationsDiff appState annotations annotations
    in
    div []
        (fieldDiff ++ [ tagsDiff, answersDiff, choicesDiff, itemTemplateQuestionsDiff, referencesDiff, expertsDiff, annotationsDiff ])


getIntegrationName : KnowledgeModel -> String -> String
getIntegrationName km integrationUuid =
    KnowledgeModel.getIntegration integrationUuid km
        |> Maybe.map Integration.getName
        |> Maybe.withDefault ""


viewAddAnswerDiff : AppState -> KnowledgeModel -> AddAnswerEventData -> Html Msg
viewAddAnswerDiff appState km event =
    let
        metrics =
            KnowledgeModel.getMetrics km

        fieldsDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Label" appState.locale
                    , gettext "Advice" appState.locale
                    ]
                    [ event.label
                    , event.advice |> Maybe.withDefault ""
                    ]

        metricsDiff =
            viewAddedChildren (gettext "Metrics" appState.locale) <|
                List.map (metricMeasureToString metrics) event.metricMeasures

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldsDiff ++ [ metricsDiff, annotationsDiff ])


viewEditAnswerDiff : AppState -> KnowledgeModel -> EditAnswerEventData -> Answer -> Html Msg
viewEditAnswerDiff appState km event answer =
    let
        metrics =
            KnowledgeModel.getMetrics km

        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Label" appState.locale
                    , gettext "Advice" appState.locale
                    ]
                    [ answer.label
                    , Maybe.withDefault "" answer.advice
                    ]
                    [ EventField.getValueWithDefault event.label answer.label
                    , EventField.getValueWithDefault event.advice answer.advice |> Maybe.withDefault ""
                    ]

        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        originalQuestions =
            List.map Question.getUuid questions

        questionNames =
            Dict.fromList <| List.map (\q -> ( Question.getUuid q, Question.getTitle q )) questions

        questionsDiff =
            viewDiffChildren (gettext "Questions" appState.locale)
                originalQuestions
                (EventField.getValueWithDefault event.followUpUuids originalQuestions)
                questionNames

        originalMetrics =
            List.map (metricMeasureToString metrics) answer.metricMeasures

        newMetrics =
            EventField.getValueWithDefault event.metricMeasures answer.metricMeasures
                |> List.map (metricMeasureToString metrics)

        metricsPropsDiff =
            viewAddedAndDeletedChildren (gettext "Metrics" appState.locale) originalMetrics newMetrics

        annotationsDiff =
            viewAnnotationsDiff appState answer.annotations (EventField.getValueWithDefault event.annotations answer.annotations)
    in
    div []
        (fieldDiff ++ [ questionsDiff, metricsPropsDiff, annotationsDiff ])


viewDeleteAnswerDiff : AppState -> KnowledgeModel -> Answer -> Html Msg
viewDeleteAnswerDiff appState km answer =
    let
        metrics =
            KnowledgeModel.getMetrics km

        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Label" appState.locale
                    , gettext "Advice" appState.locale
                    ]
                    [ answer.label
                    , answer.advice |> Maybe.withDefault ""
                    ]

        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        questionNames =
            List.map Question.getTitle questions

        questionsDiff =
            viewDeletedChildren (gettext "Questions" appState.locale) questionNames

        originalMetrics =
            List.map (metricMeasureToString metrics) answer.metricMeasures

        metricsDiff =
            viewDeletedChildren (gettext "Metrics" appState.locale) originalMetrics

        annotationsDiff =
            viewAnnotationsDiff appState answer.annotations []
    in
    div []
        (fieldDiff ++ [ questionsDiff, metricsDiff, annotationsDiff ])


viewMoveAnswer : AppState -> KnowledgeModel -> Answer -> Html Msg
viewMoveAnswer appState km answer =
    let
        metrics =
            KnowledgeModel.getMetrics km

        fieldDiff =
            viewPlain <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Label" appState.locale
                    , gettext "Advice" appState.locale
                    ]
                    [ answer.label
                    , answer.advice |> Maybe.withDefault ""
                    ]

        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        questionNames =
            List.map Question.getTitle questions

        questionsDiff =
            viewPlainChildren (gettext "Questions" appState.locale) questionNames

        originalMetrics =
            List.map (metricMeasureToString metrics) answer.metricMeasures

        metricsDiff =
            viewPlainChildren (gettext "Metrics" appState.locale) originalMetrics

        annotationsDiff =
            viewAnnotationsDiff appState answer.annotations answer.annotations
    in
    div []
        (fieldDiff ++ [ questionsDiff, metricsDiff, annotationsDiff ])


metricMeasureToString : List Metric -> MetricMeasure -> String
metricMeasureToString metrics metricMeasure =
    let
        metricName m =
            List.filter (.uuid >> (==) m.metricUuid) metrics
                |> List.head
                |> Maybe.map .title
                |> Maybe.withDefault ""
    in
    format "%s (weight = %s, measure = %s)"
        [ metricName metricMeasure
        , String.fromFloat metricMeasure.weight
        , String.fromFloat metricMeasure.measure
        ]


viewAddChoiceDiff : AppState -> AddChoiceEventData -> Html Msg
viewAddChoiceDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Label" appState.locale
                    ]
                    [ event.label
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditChoiceDiff : AppState -> EditChoiceEventData -> Choice -> Html Msg
viewEditChoiceDiff appState event choice =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Label" appState.locale
                    ]
                    [ choice.label
                    ]
                    [ EventField.getValueWithDefault event.label choice.label
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState choice.annotations (EventField.getValueWithDefault event.annotations choice.annotations)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteChoiceDiff : AppState -> Choice -> Html Msg
viewDeleteChoiceDiff appState choice =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Label" appState.locale
                    ]
                    [ choice.label
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState choice.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewMoveChoice : AppState -> Choice -> Html Msg
viewMoveChoice appState choice =
    let
        fieldDiff =
            viewPlain <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Label" appState.locale
                    ]
                    [ choice.label
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState choice.annotations choice.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddReferenceDiff : AppState -> AddReferenceEventData -> Html Msg
viewAddReferenceDiff appState =
    AddReferenceEventData.map
        (viewAddResourcePageReferenceDiff appState)
        (viewAddURLReferenceDiff appState)
        (viewAddCrossReferenceDiff appState)


viewAddResourcePageReferenceDiff : AppState -> AddReferenceResourcePageEventData -> Html Msg
viewAddResourcePageReferenceDiff appState data =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Resource Page UUID" appState.locale
                    ]
                    [ gettext "Resource Page" appState.locale
                    , data.resourcePageUuid
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] data.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddURLReferenceDiff : AppState -> AddReferenceURLEventData -> Html Msg
viewAddURLReferenceDiff appState data =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "URL" appState.locale
                    , gettext "Label" appState.locale
                    ]
                    [ gettext "URL" appState.locale
                    , data.url
                    , data.label
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] data.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddCrossReferenceDiff : AppState -> AddReferenceCrossEventData -> Html Msg
viewAddCrossReferenceDiff appState data =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Target UUID" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ gettext "Cross Reference" appState.locale
                    , data.targetUuid
                    , data.description
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] data.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditReferenceDiff : AppState -> EditReferenceEventData -> Reference -> Html Msg
viewEditReferenceDiff appState event reference =
    case ( event, reference ) of
        ( EditReferenceResourcePageEvent eventData, ResourcePageReference referenceData ) ->
            let
                fieldDiff =
                    viewDiff <|
                        List.map3 (\a b c -> ( a, b, c ))
                            [ gettext "Reference Type" appState.locale
                            , gettext "Resource Page UUID" appState.locale
                            ]
                            [ gettext "Resource Page" appState.locale
                            , referenceData.resourcePageUuid
                            ]
                            [ gettext "Resource Page" appState.locale
                            , EventField.getValueWithDefault eventData.resourcePageUuid referenceData.resourcePageUuid
                            ]

                annotationsDiff =
                    viewAnnotationsDiff appState referenceData.annotations (EventField.getValueWithDefault eventData.annotations referenceData.annotations)
            in
            div [] (fieldDiff ++ [ annotationsDiff ])

        ( EditReferenceURLEvent eventData, URLReference referenceData ) ->
            let
                fieldDiff =
                    viewDiff <|
                        List.map3 (\a b c -> ( a, b, c ))
                            [ gettext "Reference Type" appState.locale
                            , gettext "URL" appState.locale
                            , gettext "Label" appState.locale
                            ]
                            [ gettext "URL" appState.locale
                            , referenceData.url
                            , referenceData.label
                            ]
                            [ gettext "URL" appState.locale
                            , EventField.getValueWithDefault eventData.url referenceData.url
                            , EventField.getValueWithDefault eventData.label referenceData.label
                            ]

                annotationsDiff =
                    viewAnnotationsDiff appState referenceData.annotations (EventField.getValueWithDefault eventData.annotations referenceData.annotations)
            in
            div [] (fieldDiff ++ [ annotationsDiff ])

        ( EditReferenceCrossEvent eventData, CrossReference referenceData ) ->
            let
                fieldDiff =
                    viewDiff <|
                        List.map3 (\a b c -> ( a, b, c ))
                            [ gettext "Reference Type" appState.locale
                            , gettext "Target UUID" appState.locale
                            , gettext "Description" appState.locale
                            ]
                            [ gettext "Cross Reference" appState.locale
                            , referenceData.targetUuid
                            , referenceData.description
                            ]
                            [ gettext "Cross Reference" appState.locale
                            , EventField.getValueWithDefault eventData.targetUuid referenceData.targetUuid
                            , EventField.getValueWithDefault eventData.description referenceData.description
                            ]

                annotationsDiff =
                    viewAnnotationsDiff appState referenceData.annotations (EventField.getValueWithDefault eventData.annotations referenceData.annotations)
            in
            div [] (fieldDiff ++ [ annotationsDiff ])

        ( otherEvent, otherReference ) ->
            let
                deleteReference =
                    viewDeleteReferenceDiff appState otherReference

                addReference =
                    EditReferenceEventData.map
                        (viewEditResourcePageReferenceDiff appState)
                        (viewEditURLReferenceDiff appState)
                        (viewEditCrossReferenceDiff appState)
                        otherEvent
            in
            div [] [ deleteReference, addReference ]


viewEditResourcePageReferenceDiff : AppState -> EditReferenceResourcePageEventData -> Html Msg
viewEditResourcePageReferenceDiff appState data =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Resource Page UUID" appState.locale
                    ]
                    [ gettext "Resource Page" appState.locale
                    , EventField.getValueWithDefault data.resourcePageUuid ""
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] (EventField.getValueWithDefault data.annotations [])
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditURLReferenceDiff : AppState -> EditReferenceURLEventData -> Html Msg
viewEditURLReferenceDiff appState data =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "URL" appState.locale
                    , gettext "Label" appState.locale
                    ]
                    [ gettext "URL" appState.locale
                    , EventField.getValueWithDefault data.url ""
                    , EventField.getValueWithDefault data.label ""
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] (EventField.getValueWithDefault data.annotations [])
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditCrossReferenceDiff : AppState -> EditReferenceCrossEventData -> Html Msg
viewEditCrossReferenceDiff appState data =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Target UUID" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ gettext "Cross Reference" appState.locale
                    , EventField.getValueWithDefault data.targetUuid ""
                    , EventField.getValueWithDefault data.description ""
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] (EventField.getValueWithDefault data.annotations [])
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteReferenceDiff : AppState -> Reference -> Html Msg
viewDeleteReferenceDiff appState =
    Reference.map
        (viewDeleteResourcePageReferenceDiff appState)
        (viewDeleteURLReferenceDiff appState)
        (viewDeleteCrossReferenceDiff appState)


viewDeleteResourcePageReferenceDiff : AppState -> ResourcePageReferenceData -> Html Msg
viewDeleteResourcePageReferenceDiff appState data =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Resource Page UUID" appState.locale
                    ]
                    [ gettext "Resource Page" appState.locale
                    , data.resourcePageUuid
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState data.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteURLReferenceDiff : AppState -> URLReferenceData -> Html Msg
viewDeleteURLReferenceDiff appState data =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "URL" appState.locale
                    , gettext "Label" appState.locale
                    ]
                    [ gettext "URL" appState.locale
                    , data.url
                    , data.label
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState data.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteCrossReferenceDiff : AppState -> CrossReferenceData -> Html Msg
viewDeleteCrossReferenceDiff appState data =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Target UUID" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ gettext "Cross Reference" appState.locale
                    , data.targetUuid
                    , data.description
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState data.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewMoveReference : AppState -> Reference -> Html Msg
viewMoveReference appState =
    Reference.map
        (viewMoveResourcePageReference appState)
        (viewMoveURLReference appState)
        (viewMoveCrossReference appState)


viewMoveResourcePageReference : AppState -> ResourcePageReferenceData -> Html Msg
viewMoveResourcePageReference appState data =
    let
        fieldDiff =
            viewPlain <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Resource Page UUID" appState.locale
                    ]
                    [ gettext "Resource Page" appState.locale
                    , data.resourcePageUuid
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState data.annotations data.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewMoveURLReference : AppState -> URLReferenceData -> Html Msg
viewMoveURLReference appState data =
    let
        fieldDiff =
            viewPlain <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "URL" appState.locale
                    , gettext "Label" appState.locale
                    ]
                    [ gettext "URL" appState.locale
                    , data.url
                    , data.label
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState data.annotations data.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewMoveCrossReference : AppState -> CrossReferenceData -> Html Msg
viewMoveCrossReference appState data =
    let
        fieldDiff =
            viewPlain <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Reference Type" appState.locale
                    , gettext "Target UUID" appState.locale
                    , gettext "Description" appState.locale
                    ]
                    [ gettext "Cross Reference" appState.locale
                    , data.targetUuid
                    , data.description
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState data.annotations data.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddExpertDiff : AppState -> AddExpertEventData -> Html Msg
viewAddExpertDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Name" appState.locale
                    , gettext "Email" appState.locale
                    ]
                    [ event.name
                    , event.email
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditExpertDiff : AppState -> EditExpertEventData -> Expert -> Html Msg
viewEditExpertDiff appState event expert =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Name" appState.locale
                    , gettext "Email" appState.locale
                    ]
                    [ expert.name
                    , expert.email
                    ]
                    [ EventField.getValueWithDefault event.name expert.name
                    , EventField.getValueWithDefault event.email expert.email
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState expert.annotations (EventField.getValueWithDefault event.annotations expert.annotations)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteExpertDiff : AppState -> Expert -> Html Msg
viewDeleteExpertDiff appState expert =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Name" appState.locale
                    , gettext "Email" appState.locale
                    ]
                    [ expert.name
                    , expert.email
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState expert.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewMoveExpert : AppState -> Expert -> Html Msg
viewMoveExpert appState expert =
    let
        fieldDiff =
            viewPlain <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Name" appState.locale
                    , gettext "Email" appState.locale
                    ]
                    [ expert.name
                    , expert.email
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState expert.annotations expert.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddResourceCollectionDiff : AppState -> AddResourceCollectionEventData -> Html Msg
viewAddResourceCollectionDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    ]
                    [ event.title
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditResourceCollectionDiff : AppState -> KnowledgeModel -> EditResourceCollectionEventData -> ResourceCollection -> Html Msg
viewEditResourceCollectionDiff appState km event resourceCollection =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Title" appState.locale
                    ]
                    [ resourceCollection.title
                    ]
                    [ EventField.getValueWithDefault event.title resourceCollection.title
                    ]

        resourcePages =
            KnowledgeModel.getResourceCollectionResourcePages resourceCollection.uuid km

        originalResourcePages =
            List.map .uuid resourcePages

        resourcePageNames =
            Dict.fromList <| List.map (\r -> ( r.uuid, r.title )) resourcePages

        resourcePagesDiff =
            viewDiffChildren (gettext "Resource Pages" appState.locale)
                originalResourcePages
                (EventField.getValueWithDefault event.resourcePageUuids originalResourcePages)
                resourcePageNames

        annotationsDiff =
            viewAnnotationsDiff appState resourceCollection.annotations (EventField.getValueWithDefault event.annotations resourceCollection.annotations)
    in
    div [] (fieldDiff ++ [ resourcePagesDiff, annotationsDiff ])


viewDeleteResourceCollectionDiff : AppState -> KnowledgeModel -> ResourceCollection -> Html Msg
viewDeleteResourceCollectionDiff appState km resourceCollection =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    ]
                    [ resourceCollection.title
                    ]

        resourcePages =
            KnowledgeModel.getResourceCollectionResourcePages resourceCollection.uuid km

        resourcePageNames =
            List.map .title resourcePages

        resourcePagesDiff =
            viewDeletedChildren (gettext "Resource Pages" appState.locale) resourcePageNames

        annotationsDiff =
            viewAnnotationsDiff appState resourceCollection.annotations []
    in
    div [] (fieldDiff ++ [ resourcePagesDiff, annotationsDiff ])


viewAddResourcePageDiff : AppState -> AddResourcePageEventData -> Html Msg
viewAddResourcePageDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Content" appState.locale
                    ]
                    [ event.title
                    , event.content
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditResourcePageDiff : AppState -> EditResourcePageEventData -> ResourcePage -> Html Msg
viewEditResourcePageDiff appState event resourcePage =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ gettext "Title" appState.locale
                    , gettext "Content" appState.locale
                    ]
                    [ resourcePage.title
                    , resourcePage.content
                    ]
                    [ EventField.getValueWithDefault event.title resourcePage.title
                    , EventField.getValueWithDefault event.content resourcePage.content
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState resourcePage.annotations (EventField.getValueWithDefault event.annotations resourcePage.annotations)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteResourcePageDiff : AppState -> ResourcePage -> Html Msg
viewDeleteResourcePageDiff appState resourcePage =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ gettext "Title" appState.locale
                    , gettext "Content" appState.locale
                    ]
                    [ resourcePage.title
                    , resourcePage.content
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState resourcePage.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDiff : List ( String, String, String ) -> List (Html Msg)
viewDiff changes =
    List.map
        (\( fieldName, originalValue, newValue ) ->
            let
                content =
                    if originalValue == newValue then
                        [ div [ class "form-value" ] [ text originalValue ] ]

                    else
                        [ div [ class "form-value" ]
                            [ div [] [ del [] [ text originalValue ] ]
                            , div [] [ ins [] [ text newValue ] ]
                            ]
                        ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        changes


viewAdd : List ( String, String ) -> List (Html Msg)
viewAdd changes =
    List.map
        (\( fieldName, newValue ) ->
            let
                content =
                    [ div [ class "form-value" ]
                        [ div [] [ ins [] [ text newValue ] ]
                        ]
                    ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        changes


viewDelete : List ( String, String ) -> List (Html Msg)
viewDelete changes =
    List.map
        (\( fieldName, newValue ) ->
            let
                content =
                    [ div [ class "form-value" ]
                        [ div [] [ del [] [ text newValue ] ]
                        ]
                    ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        changes


viewPlain : List ( String, String ) -> List (Html Msg)
viewPlain fields =
    List.map
        (\( fieldName, newValue ) ->
            let
                content =
                    [ div [ class "form-value" ]
                        [ div [] [ span [] [ text newValue ] ]
                        ]
                    ]
            in
            div [ class "form-group" ]
                (label [ class "control-label" ] [ text fieldName ] :: content)
        )
        fields


viewDiffChildren : String -> List String -> List String -> Dict String String -> Html Msg
viewDiffChildren fieldName originalOrder newOrder childrenNames =
    let
        viewChildren ulClass uuids =
            ul [ class ulClass ]
                (List.map
                    (\uuid ->
                        Dict.get uuid childrenNames
                            |> Maybe.map (text >> List.singleton >> li [])
                            |> Maybe.withDefault emptyNode
                    )
                    uuids
                )

        diff =
            if List.isEmpty originalOrder && List.isEmpty newOrder then
                div [ class "form-value" ] [ text "-" ]

            else if originalOrder == newOrder then
                div [ class "form-value" ]
                    [ viewChildren "" originalOrder
                    ]

            else
                div [ class "form-value" ]
                    [ viewChildren "del" originalOrder
                    , viewChildren "ins" newOrder
                    ]
    in
    childrenView fieldName diff


viewAddedChildren : String -> List String -> Html Msg
viewAddedChildren fieldName children =
    childrenView fieldName <|
        if List.isEmpty children then
            div [ class "form-value" ] [ text "-" ]

        else
            ul [ class "ins" ]
                (List.map (\child -> li [] [ text child ]) children)


viewDeletedChildren : String -> List String -> Html Msg
viewDeletedChildren fieldName children =
    childrenView fieldName <|
        if List.isEmpty children then
            div [ class "form-value" ] [ text "-" ]

        else
            ul [ class "del" ]
                (List.map (\child -> li [] [ text child ]) children)


viewPlainChildren : String -> List String -> Html Msg
viewPlainChildren fieldName children =
    childrenView fieldName <|
        if List.isEmpty children then
            div [ class "form-value" ] [ text "-" ]

        else
            ul []
                (List.map (\child -> li [] [ text child ]) children)


viewAddedAndDeletedChildren : String -> List String -> List String -> Html Msg
viewAddedAndDeletedChildren fieldName originalChildren newChildren =
    childrenView fieldName <|
        if List.isEmpty originalChildren && List.isEmpty newChildren then
            div [ class "form-value" ] [ text "-" ]

        else if originalChildren == newChildren then
            ul []
                (List.map (\child -> li [] [ text child ]) originalChildren)

        else
            let
                original =
                    if List.length originalChildren > 0 then
                        ul [ class "del" ]
                            (List.map (\child -> li [] [ text child ]) originalChildren)

                    else
                        emptyNode

                new =
                    if List.length newChildren > 0 then
                        ul [ class "ins" ]
                            (List.map (\child -> li [] [ text child ]) newChildren)

                    else
                        emptyNode
            in
            div [] [ original, new ]


childrenView : String -> Html Msg -> Html Msg
childrenView fieldName diffView =
    div [ class "form-group" ]
        [ label [ class "control-label" ]
            [ text fieldName ]
        , div [ class "form-value" ]
            [ diffView ]
        ]


viewAnnotationsDiff : AppState -> List Annotation -> List Annotation -> Html msg
viewAnnotationsDiff appState originalAnnotations currentAnnotations =
    let
        isDeletedAnnotation annotation =
            not <| List.any (.key >> (==) annotation.key) currentAnnotations

        isAddedAnnotation annotation =
            not <| List.any (.key >> (==) annotation.key) originalAnnotations

        isEditedAnnotation annotation =
            case List.find (.key >> (==) annotation.key) originalAnnotations of
                Just originalAnnotation ->
                    originalAnnotation.value /= annotation.value

                Nothing ->
                    False

        cssClass annotation =
            if isDeletedAnnotation annotation then
                "del"

            else if isAddedAnnotation annotation then
                "ins"

            else if isEditedAnnotation annotation then
                "edited"

            else
                ""

        viewAnnotation annotation =
            [ dt [ class (cssClass annotation) ] [ text annotation.key ]
            , dd [ class (cssClass annotation) ] [ text annotation.value ]
            ]

        deletedAnnotationsView =
            List.filter isDeletedAnnotation originalAnnotations
                |> List.concatMap viewAnnotation

        currentAnnotationsView =
            List.concatMap viewAnnotation currentAnnotations

        allAnnotationsView =
            deletedAnnotationsView ++ currentAnnotationsView

        valueView =
            if List.isEmpty allAnnotationsView then
                text "-"

            else
                dl [] allAnnotationsView
    in
    div [ class "form-group" ]
        [ label [ class "control-label" ]
            [ text (gettext "Annotations" appState.locale) ]
        , div [ class "form-value" ]
            [ valueView ]
        ]


formActions : AppState -> Model -> Html Msg
formActions appState model =
    let
        ( rejectLabel, rejectDisabled ) =
            actionState appState model RejectButtonClicked (gettext "Reject" appState.locale)

        ( applyLabel, applyDisabled ) =
            actionState appState model ApplyButtonClicked (gettext "Apply" appState.locale)

        ( applyAllLabel, applyAllDisabled ) =
            actionState appState model ApplyAllButtonClicked (gettext "Apply all" appState.locale)
    in
    div [ class "form-actions" ]
        [ button [ class "btn btn-warning btn-with-loader", onClick RejectEvent, rejectDisabled, dataCy "km-migration_reject-button" ]
            [ rejectLabel ]
        , div []
            [ button [ class "btn btn-success btn-with-loader", onClick ApplyEvent, applyDisabled, dataCy "km-migration_apply-button" ]
                [ applyLabel ]
            , button [ class "btn btn-success btn-with-loader", onClick ApplyAll, applyAllDisabled, dataCy "km-migration_apply-all-button" ]
                [ applyAllLabel ]
            ]
        ]


actionState : AppState -> Model -> ButtonClicked -> String -> ( Html msg, Html.Attribute msg )
actionState appState model buttonClicked defaultLabel =
    let
        disabledAttribute =
            disabled <|
                case model.conflict of
                    Loading ->
                        True

                    _ ->
                        False

        labelHtml =
            if ActionResult.isLoading model.conflict && model.buttonClicked == Just buttonClicked then
                ActionButton.loader appState

            else
                text defaultLabel
    in
    ( labelHtml, disabledAttribute )


viewCompletedMigration : AppState -> Model -> Html Msg
viewCompletedMigration appState model =
    div [ class "col-xs-12" ]
        [ div [ class "p-5 mb-4 bg-light rounded-3 full-page-error", dataCy "km-migration_completed" ]
            [ h1 [ class "display-3" ] [ faSet "_global.success" appState ]
            , p [ class "fs-4" ]
                [ text (gettext "Migration successfully completed." appState.locale)
                , br [] []
                , text (gettext "You can publish the new version now." appState.locale)
                ]
            , linkTo appState
                (Routes.kmEditorPublish model.branchUuid)
                [ class "btn btn-primary btn-lg with-icon-after"
                , dataCy "km-migration_publish-button"
                ]
                [ text (gettext "Publish" appState.locale)
                , faSet "_global.arrowRight" appState
                ]
            ]
        ]
