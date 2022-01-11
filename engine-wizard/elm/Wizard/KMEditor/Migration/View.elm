module Wizard.KMEditor.Migration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
import Html exposing (Html, br, button, code, dd, del, div, dl, dt, h1, h3, ins, label, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import List.Extra as List
import Shared.Data.Event as Event exposing (Event(..))
import Shared.Data.Event.AddAnswerEventData exposing (AddAnswerEventData)
import Shared.Data.Event.AddChapterEventData exposing (AddChapterEventData)
import Shared.Data.Event.AddChoiceEventData exposing (AddChoiceEventData)
import Shared.Data.Event.AddExpertEventData exposing (AddExpertEventData)
import Shared.Data.Event.AddIntegrationEventData exposing (AddIntegrationEventData)
import Shared.Data.Event.AddMetricEventData exposing (AddMetricEventData)
import Shared.Data.Event.AddPhaseEventData exposing (AddPhaseEventData)
import Shared.Data.Event.AddQuestionEventData as AddQuestionEventQuestion exposing (AddQuestionEventData(..))
import Shared.Data.Event.AddReferenceCrossEventData exposing (AddReferenceCrossEventData)
import Shared.Data.Event.AddReferenceEventData as AddReferenceEventData exposing (AddReferenceEventData)
import Shared.Data.Event.AddReferenceResourcePageEventData exposing (AddReferenceResourcePageEventData)
import Shared.Data.Event.AddReferenceURLEventData exposing (AddReferenceURLEventData)
import Shared.Data.Event.AddTagEventData exposing (AddTagEventData)
import Shared.Data.Event.EditAnswerEventData exposing (EditAnswerEventData)
import Shared.Data.Event.EditChapterEventData exposing (EditChapterEventData)
import Shared.Data.Event.EditChoiceEventData exposing (EditChoiceEventData)
import Shared.Data.Event.EditExpertEventData exposing (EditExpertEventData)
import Shared.Data.Event.EditIntegrationEventData exposing (EditIntegrationEventData)
import Shared.Data.Event.EditKnowledgeModelEventData exposing (EditKnowledgeModelEventData)
import Shared.Data.Event.EditMetricEventData exposing (EditMetricEventData)
import Shared.Data.Event.EditPhaseEventData exposing (EditPhaseEventData)
import Shared.Data.Event.EditQuestionEventData as EditQuestionEventData exposing (EditQuestionEventData(..))
import Shared.Data.Event.EditReferenceCrossEventData exposing (EditReferenceCrossEventData)
import Shared.Data.Event.EditReferenceEventData as EditReferenceEventData exposing (EditReferenceEventData(..))
import Shared.Data.Event.EditReferenceResourcePageEventData exposing (EditReferenceResourcePageEventData)
import Shared.Data.Event.EditReferenceURLEventData exposing (EditReferenceURLEventData)
import Shared.Data.Event.EditTagEventData exposing (EditTagEventData)
import Shared.Data.Event.EventField as EventField
import Shared.Data.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import Shared.Data.KnowledgeModel.Annotation exposing (Annotation)
import Shared.Data.KnowledgeModel.Answer exposing (Answer)
import Shared.Data.KnowledgeModel.Chapter exposing (Chapter)
import Shared.Data.KnowledgeModel.Choice exposing (Choice)
import Shared.Data.KnowledgeModel.Expert exposing (Expert)
import Shared.Data.KnowledgeModel.Integration exposing (Integration)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Data.KnowledgeModel.Question as Question exposing (Question(..))
import Shared.Data.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Shared.Data.KnowledgeModel.Reference.CrossReferenceData exposing (CrossReferenceData)
import Shared.Data.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import Shared.Data.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)
import Shared.Data.KnowledgeModel.Tag exposing (Tag)
import Shared.Data.Migration exposing (Migration)
import Shared.Data.Migration.MigrationState.MigrationStateType exposing (MigrationStateType(..))
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (lg, lh, lx)
import String.Format exposing (format)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Migration.Models exposing (Model)
import Wizard.KMEditor.Migration.Msgs exposing (Msg(..))
import Wizard.KMEditor.Migration.View.DiffTree as DiffTree
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Routes as Routes


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.KMEditor.Migration.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Migration.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (migrationView appState model) model.migration


migrationView : AppState -> Model -> Migration -> Html Msg
migrationView appState model migration =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ lx_ "stateError" appState ]

        runningStateMessage =
            div [ class "alert alert-warning" ]
                [ lx_ "running" appState ]

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
                    runningStateMessage

                _ ->
                    errorMessage
    in
    div [ class "col KMEditor__Migration", dataCy "km-editor_migration" ]
        [ div [] [ Page.header (lg "kmMigration" appState) [] ]
        , FormResult.view appState model.conflict
        , currentView
        ]


migrationSummary : AppState -> Migration -> Html Msg
migrationSummary appState migration =
    div [ class "col-12" ]
        [ p []
            (lh_ "summary"
                [ strong [] [ text migration.branchName ]
                , code [] [ text migration.branchPreviousPackageId ]
                , code [] [ text migration.targetPackageId ]
                ]
                appState
            )
        ]


getEventView : AppState -> Model -> Migration -> Event -> Html Msg
getEventView appState model migration event =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ lx_ "eventError" appState ]
    in
    case event of
        AddKnowledgeModelEvent _ _ ->
            -- AddKnowledgeModelEvent should never appear in migrations
            emptyNode

        EditKnowledgeModelEvent eventData _ ->
            migration.currentKnowledgeModel
                |> viewEditKnowledgeModelDiff appState eventData
                |> viewEvent appState model event (lg "event.editKM" appState)

        AddMetricEvent eventData _ ->
            viewAddMetricDiff appState eventData
                |> viewEvent appState model event (lg "event.addMetric" appState)

        EditMetricEvent eventData commonData ->
            KnowledgeModel.getMetric commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditMetricDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editMetric" appState))
                |> Maybe.withDefault errorMessage

        DeleteMetricEvent commonData ->
            KnowledgeModel.getMetric commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteMetricDiff appState)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteMetric" appState))
                |> Maybe.withDefault errorMessage

        AddPhaseEvent eventData _ ->
            viewAddPhaseDiff appState eventData
                |> viewEvent appState model event (lg "event.addPhase" appState)

        EditPhaseEvent eventData commonData ->
            KnowledgeModel.getPhase commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditPhaseDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editPhase" appState))
                |> Maybe.withDefault errorMessage

        DeletePhaseEvent commonData ->
            KnowledgeModel.getPhase commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeletePhaseDiff appState)
                |> Maybe.map (viewEvent appState model event (lg "event.deletePhase" appState))
                |> Maybe.withDefault errorMessage

        AddTagEvent eventData _ ->
            viewAddTagDiff appState eventData
                |> viewEvent appState model event (lg "event.addTag" appState)

        EditTagEvent eventData commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditTagDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editTag" appState))
                |> Maybe.withDefault errorMessage

        DeleteTagEvent commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteTagDiff appState)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteTag" appState))
                |> Maybe.withDefault errorMessage

        AddIntegrationEvent eventData _ ->
            viewAddIntegrationDiff appState eventData
                |> viewEvent appState model event (lg "event.addIntegration" appState)

        EditIntegrationEvent eventData commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditIntegrationDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editIntegration" appState))
                |> Maybe.withDefault errorMessage

        DeleteIntegrationEvent commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteIntegrationDiff appState)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteIntegration" appState))
                |> Maybe.withDefault errorMessage

        AddChapterEvent eventData _ ->
            viewAddChapterDiff appState eventData
                |> viewEvent appState model event (lg "event.addChapter" appState)

        EditChapterEvent eventData commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditChapterDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editChapter" appState))
                |> Maybe.withDefault errorMessage

        DeleteChapterEvent commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteChapterDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteChapter" appState))
                |> Maybe.withDefault errorMessage

        AddQuestionEvent eventData _ ->
            viewAddQuestionDiff appState migration.currentKnowledgeModel eventData
                |> viewEvent appState model event (lg "event.addQuestion" appState)

        EditQuestionEvent eventData commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditQuestionDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editQuestion" appState))
                |> Maybe.withDefault errorMessage

        DeleteQuestionEvent commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteQuestionDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteQuestion" appState))
                |> Maybe.withDefault errorMessage

        AddAnswerEvent eventData _ ->
            viewAddAnswerDiff appState migration.currentKnowledgeModel eventData
                |> viewEvent appState model event (lg "event.addAnswer" appState)

        EditAnswerEvent eventData commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditAnswerDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editAnswer" appState))
                |> Maybe.withDefault errorMessage

        DeleteAnswerEvent commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteAnswerDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteAnswer" appState))
                |> Maybe.withDefault errorMessage

        AddChoiceEvent eventData _ ->
            viewAddChoiceDiff appState eventData
                |> viewEvent appState model event (lg "event.addChoice" appState)

        EditChoiceEvent eventData commonData ->
            KnowledgeModel.getChoice commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditChoiceDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editChoice" appState))
                |> Maybe.withDefault errorMessage

        DeleteChoiceEvent commonData ->
            KnowledgeModel.getChoice commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteChoiceDiff appState)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteChoice" appState))
                |> Maybe.withDefault errorMessage

        AddReferenceEvent eventData _ ->
            viewAddReferenceDiff appState eventData
                |> viewEvent appState model event (lg "event.addReference" appState)

        EditReferenceEvent eventData commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditReferenceDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editReference" appState))
                |> Maybe.withDefault errorMessage

        DeleteReferenceEvent commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteReferenceDiff appState)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteReference" appState))
                |> Maybe.withDefault errorMessage

        AddExpertEvent eventData _ ->
            viewAddExpertDiff appState eventData
                |> viewEvent appState model event (lg "event.addExpert" appState)

        EditExpertEvent eventData commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditExpertDiff appState eventData)
                |> Maybe.map (viewEvent appState model event (lg "event.editExpert" appState))
                |> Maybe.withDefault errorMessage

        DeleteExpertEvent commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteExpertDiff appState)
                |> Maybe.map (viewEvent appState model event (lg "event.deleteExpert" appState))
                |> Maybe.withDefault errorMessage

        MoveQuestionEvent _ commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveQuestion appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (lg "event.moveQuestion" appState))
                |> Maybe.withDefault errorMessage

        MoveAnswerEvent _ commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveAnswer appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model event (lg "event.moveAnswer" appState))
                |> Maybe.withDefault errorMessage

        MoveChoiceEvent _ commonData ->
            KnowledgeModel.getChoice commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveChoice appState)
                |> Maybe.map (viewEvent appState model event (lg "event.moveChoice" appState))
                |> Maybe.withDefault errorMessage

        MoveReferenceEvent _ commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveReference appState)
                |> Maybe.map (viewEvent appState model event (lg "event.moveReference" appState))
                |> Maybe.withDefault errorMessage

        MoveExpertEvent _ commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewMoveExpert appState)
                |> Maybe.map (viewEvent appState model event (lg "event.moveExpert" appState))
                |> Maybe.withDefault errorMessage


viewEvent : AppState -> Model -> Event -> String -> Html Msg -> Html Msg
viewEvent appState model event name diffView =
    div [ dataCy ("km-migration_event_" ++ Event.getUuid event) ]
        [ h3 [] [ text name ]
        , div [ class "card bg-light" ]
            [ div [ class "card-body" ]
                [ diffView
                , formActions appState model
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
            viewDiffChildren (lg "chapters" appState) originalChapters (EventField.getValueWithDefault event.chapterUuids originalChapters) chapterNames

        metrics =
            KnowledgeModel.getMetrics km

        originalMetrics =
            List.map .uuid metrics

        metricNames =
            Dict.fromList <| List.map (\m -> ( m.uuid, m.title )) metrics

        metricsDiff =
            viewDiffChildren (lg "metrics" appState)
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
            viewDiffChildren (lg "phases" appState)
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
            viewDiffChildren (lg "tags" appState)
                originalTags
                (EventField.getValueWithDefault event.tagUuids originalTags)
                tagNames

        integrations =
            KnowledgeModel.getIntegrations km

        originalIntegrations =
            List.map .uuid integrations

        integrationNames =
            Dict.fromList <| List.map (\i -> ( i.uuid, i.name )) integrations

        integrationsDiff =
            viewDiffChildren (lg "integrations" appState)
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
                    [ lg "metric.title" appState
                    , lg "metric.abbreviation" appState
                    , lg "metric.description" appState
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
                    [ lg "metric.title" appState
                    , lg "metric.abbreviation" appState
                    , lg "metric.description" appState
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
                    [ lg "metric.title" appState
                    , lg "metric.abbreviation" appState
                    , lg "metric.description" appState
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
                    [ lg "phase.title" appState
                    , lg "phase.description" appState
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
                    [ lg "phase.title" appState
                    , lg "phase.description" appState
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
                    [ lg "phase.title" appState
                    , lg "phase.description" appState
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
                    [ lg "tag.name" appState
                    , lg "tag.description" appState
                    , lg "tag.color" appState
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
                    [ lg "tag.name" appState
                    , lg "tag.description" appState
                    , lg "tag.color" appState
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
                    [ lg "tag.name" appState
                    , lg "tag.description" appState
                    , lg "tag.color" appState
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
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ lg "integration.id" appState
                    , lg "integration.name" appState
                    , lg "integration.props" appState
                    , lg "integration.response.itemUrl" appState
                    , lg "integration.request.method" appState
                    , lg "integration.request.url" appState
                    , lg "integration.request.headers" appState
                    , lg "integration.request.body" appState
                    , lg "integration.response.listField" appState
                    , lg "integration.response.idField" appState
                    , lg "integration.response.itemTemplate" appState
                    ]
                    [ event.id
                    , event.name
                    , String.join ", " event.props
                    , event.responseItemUrl
                    , event.requestMethod
                    , event.requestUrl
                    , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) event.requestHeaders
                    , event.requestBody
                    , event.responseListField
                    , event.responseItemId
                    , event.responseItemTemplate
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState [] event.annotations
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewEditIntegrationDiff : AppState -> EditIntegrationEventData -> Integration -> Html Msg
viewEditIntegrationDiff appState event integration =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ lg "integration.id" appState
                    , lg "integration.name" appState
                    , lg "integration.props" appState
                    , lg "integration.response.itemUrl" appState
                    , lg "integration.request.method" appState
                    , lg "integration.request.url" appState
                    , lg "integration.request.headers" appState
                    , lg "integration.request.body" appState
                    , lg "integration.response.listField" appState
                    , lg "integration.response.idField" appState
                    , lg "integration.response.itemTemplate" appState
                    ]
                    [ integration.id
                    , integration.name
                    , String.join ", " integration.props
                    , integration.responseItemUrl
                    , integration.requestMethod
                    , integration.requestUrl
                    , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) integration.requestHeaders
                    , integration.requestBody
                    , integration.responseListField
                    , integration.responseItemId
                    , integration.responseItemTemplate
                    ]
                    [ EventField.getValueWithDefault event.id integration.id
                    , EventField.getValueWithDefault event.name integration.name
                    , String.join ", " <| EventField.getValueWithDefault event.props integration.props
                    , EventField.getValueWithDefault event.responseItemUrl integration.responseItemUrl
                    , EventField.getValueWithDefault event.requestMethod integration.requestMethod
                    , EventField.getValueWithDefault event.requestUrl integration.requestUrl
                    , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) <| EventField.getValueWithDefault event.requestHeaders integration.requestHeaders
                    , EventField.getValueWithDefault event.requestBody integration.requestBody
                    , EventField.getValueWithDefault event.responseListField integration.responseListField
                    , EventField.getValueWithDefault event.responseItemId integration.responseItemId
                    , EventField.getValueWithDefault event.responseItemTemplate integration.responseItemTemplate
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState integration.annotations (EventField.getValueWithDefault event.annotations integration.annotations)
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewDeleteIntegrationDiff : AppState -> Integration -> Html Msg
viewDeleteIntegrationDiff appState integration =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ lg "integration.id" appState
                    , lg "integration.name" appState
                    , lg "integration.props" appState
                    , lg "integration.response.itemUrl" appState
                    , lg "integration.request.method" appState
                    , lg "integration.request.url" appState
                    , lg "integration.request.headers" appState
                    , lg "integration.request.body" appState
                    , lg "integration.response.listField" appState
                    , lg "integration.response.idField" appState
                    , lg "integration.response.itemTemplate" appState
                    ]
                    [ integration.id
                    , integration.name
                    , String.join ", " integration.props
                    , integration.responseItemUrl
                    , integration.requestMethod
                    , integration.requestUrl
                    , String.join ", " <| List.map (\{ key, value } -> key ++ ": " ++ value) integration.requestHeaders
                    , integration.requestBody
                    , integration.responseListField
                    , integration.responseItemId
                    , integration.responseItemTemplate
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState integration.annotations []
    in
    div [] (fieldDiff ++ [ annotationsDiff ])


viewAddChapterDiff : AppState -> AddChapterEventData -> Html Msg
viewAddChapterDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ lg "chapter.title" appState
                    , lg "chapter.text" appState
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
                    [ lg "chapter.title" appState
                    , lg "chapter.text" appState
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
            viewDiffChildren (lg "questions" appState)
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
                    [ lg "chapter.title" appState
                    , lg "chapter.text" appState
                    ]
                    [ chapter.title
                    , Maybe.withDefault "" chapter.text
                    ]

        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km

        questionNames =
            List.map Question.getTitle questions

        questionsDiff =
            viewDeletedChildren (lg "questions" appState) questionNames

        annotationsDiff =
            viewAnnotationsDiff appState chapter.annotations []
    in
    div [] (fieldDiff ++ [ questionsDiff, annotationsDiff ])


viewAddQuestionDiff : AppState -> KnowledgeModel -> AddQuestionEventData -> Html Msg
viewAddQuestionDiff appState km event =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ lg "question.type" appState
                , lg "question.title" appState
                , lg "question.text" appState
                ]
                [ AddQuestionEventQuestion.getTypeString event
                , AddQuestionEventQuestion.map .title .title .title .title .title event
                , AddQuestionEventQuestion.map .text .text .text .text .text event |> Maybe.withDefault ""
                ]

        extraFields =
            case event of
                AddQuestionValueEvent data ->
                    [ ( lg "questionValueType" appState, QuestionValueType.toString data.valueType ) ]

                AddQuestionIntegrationEvent data ->
                    [ ( lg "integration" appState, getIntegrationName km data.integrationUuid ) ]

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
                    viewAddedChildren (lg "integration.props" appState) props

                _ ->
                    emptyNode

        tags =
            KnowledgeModel.getTags km

        tagUuids =
            AddQuestionEventQuestion.map .tagUuids .tagUuids .tagUuids .tagUuids .tagUuids event

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        originalTags =
            List.map (\_ -> "") tagUuids

        tagsDiff =
            viewDiffChildren (lg "tags" appState) originalTags tagUuids tagNames

        annotations =
            AddQuestionEventQuestion.map .annotations .annotations .annotations .annotations .annotations event

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
                [ lg "question.type" appState
                , lg "question.title" appState
                , lg "question.text" appState
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
                            [ ( lg "questionValueType" appState, originalStr, newStr ) ]

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
                            [ ( lg "integration" appState, originalStr, newStr ) ]

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
                    viewAddedAndDeletedChildren (lg "integration.props" appState) originalProps newProps

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
            viewDiffChildren (lg "tags" appState)
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
                    viewDiffChildren (lg "answers" appState)
                        originalAnswers
                        (EventField.getValueWithDefault data.answerUuids originalAnswers)
                        answerNames

                _ ->
                    emptyNode

        -- Choices
        choicesDiff =
            case question of
                MultiChoiceQuestion _ _ ->
                    viewPlainChildren (lg "choices" appState) <|
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
                    viewDiffChildren (lg "questions" appState)
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
            Dict.fromList <| List.map (\r -> ( Reference.getUuid r, Reference.getVisibleName r )) references

        referencesDiff =
            viewDiffChildren (lg "references" appState)
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
            viewDiffChildren (lg "experts" appState)
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
                [ lg "question.type" appState
                , lg "question.title" appState
                , lg "question.text" appState
                ]
                [ Question.getTypeString question
                , Question.getTitle question
                , Question.getText question |> Maybe.withDefault ""
                ]

        extraFields =
            case question of
                ValueQuestion _ data ->
                    [ ( lg "questionValueType" appState, QuestionValueType.toString data.valueType ) ]

                IntegrationQuestion _ data ->
                    [ ( lg "integration" appState, getIntegrationName km data.integrationUuid ) ]

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
            viewDeletedChildren (lg "tags" appState) tagNames

        -- Answers
        answersDiff =
            case question of
                OptionsQuestion _ _ ->
                    viewDeletedChildren (lg "answers" appState) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionAnswers questionUuid km

                _ ->
                    emptyNode

        -- Choices
        choicesDiff =
            case question of
                MultiChoiceQuestion _ _ ->
                    viewPlainChildren (lg "choices" appState) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionChoices questionUuid km

                _ ->
                    emptyNode

        -- Item Template Questions
        itemTemplateQuestionsDiff =
            case question of
                ListQuestion _ _ ->
                    viewDeletedChildren (lg "questions" appState) <|
                        List.map Question.getTitle <|
                            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km

                _ ->
                    emptyNode

        -- References
        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        referencesDiff =
            viewDeletedChildren (lg "references" appState) <| List.map Reference.getVisibleName references

        -- Experts
        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        expertsDiff =
            viewDeletedChildren (lg "experts" appState) <| List.map .name experts

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
                [ lg "question.type" appState
                , lg "question.title" appState
                , lg "question.text" appState
                ]
                [ Question.getTypeString question
                , Question.getTitle question
                , Question.getText question |> Maybe.withDefault ""
                ]

        extraFields =
            case question of
                ValueQuestion _ data ->
                    [ ( lg "questionValueType" appState, QuestionValueType.toString data.valueType ) ]

                IntegrationQuestion _ data ->
                    [ ( lg "integration" appState, getIntegrationName km data.integrationUuid ) ]

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
            viewPlainChildren (lg "tags" appState) tagNames

        -- Answers
        answersDiff =
            case question of
                OptionsQuestion _ _ ->
                    viewPlainChildren (lg "answers" appState) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionAnswers questionUuid km

                _ ->
                    emptyNode

        -- Choices
        choicesDiff =
            case question of
                MultiChoiceQuestion _ _ ->
                    viewPlainChildren (lg "choices" appState) <|
                        List.map .label <|
                            KnowledgeModel.getQuestionChoices questionUuid km

                _ ->
                    emptyNode

        -- Item Template Questions
        itemTemplateQuestionsDiff =
            case question of
                ListQuestion _ _ ->
                    viewPlainChildren (lg "questions" appState) <|
                        List.map Question.getTitle <|
                            KnowledgeModel.getQuestionItemTemplateQuestions questionUuid km

                _ ->
                    emptyNode

        -- References
        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        referencesDiff =
            viewPlainChildren (lg "references" appState) <| List.map Reference.getVisibleName references

        -- Experts
        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        expertsDiff =
            viewPlainChildren (lg "experts" appState) <| List.map .name experts

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
        |> Maybe.map .name
        |> Maybe.withDefault ""


viewAddAnswerDiff : AppState -> KnowledgeModel -> AddAnswerEventData -> Html Msg
viewAddAnswerDiff appState km event =
    let
        metrics =
            KnowledgeModel.getMetrics km

        fieldsDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ lg "answer.label" appState
                    , lg "answer.advice" appState
                    ]
                    [ event.label
                    , event.advice |> Maybe.withDefault ""
                    ]

        metricsDiff =
            viewAddedChildren (lg "metrics" appState) <|
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
                    [ lg "answer.label" appState
                    , lg "answer.advice" appState
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
            viewDiffChildren (lg "questions" appState)
                originalQuestions
                (EventField.getValueWithDefault event.followUpUuids originalQuestions)
                questionNames

        originalMetrics =
            List.map (metricMeasureToString metrics) answer.metricMeasures

        newMetrics =
            EventField.getValueWithDefault event.metricMeasures answer.metricMeasures
                |> List.map (metricMeasureToString metrics)

        metricsPropsDiff =
            viewAddedAndDeletedChildren (lg "metrics" appState) originalMetrics newMetrics

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
                    [ lg "answer.label" appState
                    , lg "answer.advice" appState
                    ]
                    [ answer.label
                    , answer.advice |> Maybe.withDefault ""
                    ]

        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        questionNames =
            List.map Question.getTitle questions

        questionsDiff =
            viewDeletedChildren (lg "questions" appState) questionNames

        originalMetrics =
            List.map (metricMeasureToString metrics) answer.metricMeasures

        metricsDiff =
            viewDeletedChildren (lg "metrics" appState) originalMetrics

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
                    [ lg "answer.label" appState
                    , lg "answer.advice" appState
                    ]
                    [ answer.label
                    , answer.advice |> Maybe.withDefault ""
                    ]

        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        questionNames =
            List.map Question.getTitle questions

        questionsDiff =
            viewPlainChildren (lg "questions" appState) questionNames

        originalMetrics =
            List.map (metricMeasureToString metrics) answer.metricMeasures

        metricsDiff =
            viewPlainChildren (lg "metrics" appState) originalMetrics

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
                    [ lg "choice.label" appState
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
                    [ lg "choice.label" appState
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
                    [ lg "choice.label" appState
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
                    [ lg "choice.label" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.shortUuid" appState
                    ]
                    [ lg "referenceType.resourcePage" appState
                    , data.shortUuid
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
                    [ lg "referenceType" appState
                    , lg "reference.url" appState
                    , lg "reference.label" appState
                    ]
                    [ lg "referenceType.url" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.targetUuid" appState
                    , lg "reference.description" appState
                    ]
                    [ lg "referenceType.cross" appState
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
                            [ lg "referenceType" appState
                            , lg "reference.shortUuid" appState
                            ]
                            [ lg "referenceType.resourcePage" appState
                            , referenceData.shortUuid
                            ]
                            [ lg "referenceType.resourcePage" appState
                            , EventField.getValueWithDefault eventData.shortUuid referenceData.shortUuid
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
                            [ lg "referenceType" appState
                            , lg "reference.url" appState
                            , lg "reference.label" appState
                            ]
                            [ lg "referenceType.url" appState
                            , referenceData.url
                            , referenceData.label
                            ]
                            [ lg "referenceType.url" appState
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
                            [ lg "referenceType" appState
                            , lg "reference.targetUuid" appState
                            , lg "reference.description" appState
                            ]
                            [ lg "referenceType.cross" appState
                            , referenceData.targetUuid
                            , referenceData.description
                            ]
                            [ lg "referenceType.cross" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.shortUuid" appState
                    ]
                    [ lg "referenceType.resourcePage" appState
                    , EventField.getValueWithDefault data.shortUuid ""
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
                    [ lg "referenceType" appState
                    , lg "reference.url" appState
                    , lg "reference.label" appState
                    ]
                    [ lg "referenceType.url" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.targetUuid" appState
                    , lg "reference.description" appState
                    ]
                    [ lg "referenceType.cross" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.shortUuid" appState
                    ]
                    [ lg "referenceType.resourcePage" appState
                    , data.shortUuid
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
                    [ lg "referenceType" appState
                    , lg "reference.url" appState
                    , lg "reference.label" appState
                    ]
                    [ lg "referenceType.url" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.targetUuid" appState
                    , lg "reference.description" appState
                    ]
                    [ lg "referenceType.cross" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.shortUuid" appState
                    ]
                    [ lg "referenceType.resourcePage" appState
                    , data.shortUuid
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
                    [ lg "referenceType" appState
                    , lg "reference.url" appState
                    , lg "reference.label" appState
                    ]
                    [ lg "referenceType.url" appState
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
                    [ lg "referenceType" appState
                    , lg "reference.targetUuid" appState
                    , lg "reference.description" appState
                    ]
                    [ lg "referenceType.cross" appState
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
                    [ lg "expert.name" appState
                    , lg "expert.email" appState
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
                    [ lg "expert.name" appState
                    , lg "expert.email" appState
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
                    [ lg "expert.name" appState
                    , lg "expert.email" appState
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
                    [ lg "expert.name" appState
                    , lg "expert.email" appState
                    ]
                    [ expert.name
                    , expert.email
                    ]

        annotationsDiff =
            viewAnnotationsDiff appState expert.annotations expert.annotations
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
            [ text (lg "annotations" appState) ]
        , div [ class "form-value" ]
            [ valueView ]
        ]


formActions : AppState -> Model -> Html Msg
formActions appState model =
    let
        actionsDisabled =
            case model.conflict of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "form-actions" ]
        [ button [ class "btn btn-warning", onClick RejectEvent, disabled actionsDisabled, dataCy "km-migration_reject-button" ]
            [ lx_ "action.reject" appState ]
        , button [ class "btn btn-success", onClick ApplyEvent, disabled actionsDisabled, dataCy "km-migration_apply-button" ]
            [ lx_ "action.apply" appState ]
        ]


viewCompletedMigration : AppState -> Model -> Html Msg
viewCompletedMigration appState model =
    div [ class "col-xs-12" ]
        [ div [ class "jumbotron full-page-error", dataCy "km-migration_completed" ]
            [ h1 [ class "display-3" ] [ faSet "_global.success" appState ]
            , p []
                [ lx_ "completed.msg1" appState
                , br [] []
                , lx_ "completed.msg2" appState
                ]
            , div [ class "text-right" ]
                [ linkTo appState
                    (Routes.KMEditorRoute <| PublishRoute model.branchUuid)
                    [ class "btn btn-primary"
                    , dataCy "km-migration_publish-button"
                    ]
                    [ lx_ "completed.publish" appState
                    , faSet "_global.arrowRight" appState
                    ]
                ]
            ]
        ]
