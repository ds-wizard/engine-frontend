module KMEditor.Migration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.AppState exposing (AppState)
import Common.Html exposing (..)
import Common.Locale exposing (l, lg, lh, lx)
import Common.View.FormResult as FormResult
import Common.View.Page as Page
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import KMEditor.Common.Events.AddAnswerEventData exposing (AddAnswerEventData)
import KMEditor.Common.Events.AddChapterEventData exposing (AddChapterEventData)
import KMEditor.Common.Events.AddExpertEventData exposing (AddExpertEventData)
import KMEditor.Common.Events.AddIntegrationEventData exposing (AddIntegrationEventData)
import KMEditor.Common.Events.AddQuestionEventData as AddQuestionEventQuestion exposing (AddQuestionEventData(..))
import KMEditor.Common.Events.AddReferenceCrossEventData exposing (AddReferenceCrossEventData)
import KMEditor.Common.Events.AddReferenceEventData as AddReferenceEventData exposing (AddReferenceEventData)
import KMEditor.Common.Events.AddReferenceResourcePageEventData exposing (AddReferenceResourcePageEventData)
import KMEditor.Common.Events.AddReferenceURLEventData exposing (AddReferenceURLEventData)
import KMEditor.Common.Events.AddTagEventData exposing (AddTagEventData)
import KMEditor.Common.Events.EditAnswerEventData exposing (EditAnswerEventData)
import KMEditor.Common.Events.EditChapterEventData exposing (EditChapterEventData)
import KMEditor.Common.Events.EditExpertEventData exposing (EditExpertEventData)
import KMEditor.Common.Events.EditIntegrationEventData exposing (EditIntegrationEventData)
import KMEditor.Common.Events.EditKnowledgeModelEventData exposing (EditKnowledgeModelEventData)
import KMEditor.Common.Events.EditQuestionEventData as EditQuestionEventData exposing (EditQuestionEventData(..))
import KMEditor.Common.Events.EditReferenceCrossEventData exposing (EditReferenceCrossEventData)
import KMEditor.Common.Events.EditReferenceEventData as EditReferenceEventData exposing (EditReferenceEventData(..))
import KMEditor.Common.Events.EditReferenceResourcePageEventData exposing (EditReferenceResourcePageEventData)
import KMEditor.Common.Events.EditReferenceURLEventData exposing (EditReferenceURLEventData)
import KMEditor.Common.Events.EditTagEventData exposing (EditTagEventData)
import KMEditor.Common.Events.Event exposing (Event(..))
import KMEditor.Common.Events.EventField as EventField
import KMEditor.Common.KnowledgeModel.Answer exposing (Answer)
import KMEditor.Common.KnowledgeModel.Chapter exposing (Chapter)
import KMEditor.Common.KnowledgeModel.Expert exposing (Expert)
import KMEditor.Common.KnowledgeModel.Integration exposing (Integration)
import KMEditor.Common.KnowledgeModel.KnowledgeModel as KnowledgeModel exposing (KnowledgeModel)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import KMEditor.Common.KnowledgeModel.MetricMeasure exposing (MetricMeasure)
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import KMEditor.Common.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import KMEditor.Common.KnowledgeModel.Reference as Reference exposing (Reference(..))
import KMEditor.Common.KnowledgeModel.Reference.CrossReferenceData exposing (CrossReferenceData)
import KMEditor.Common.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import KMEditor.Common.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)
import KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import KMEditor.Common.Migration exposing (Migration)
import KMEditor.Common.MigrationStateType exposing (MigrationStateType(..))
import KMEditor.Migration.Models exposing (Model)
import KMEditor.Migration.Msgs exposing (Msg(..))
import KMEditor.Migration.View.DiffTree as DiffTree
import KMEditor.Routes exposing (Route(..))
import Routes
import String.Format exposing (format)


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Migration.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "KMEditor.Migration.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "KMEditor.Migration.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (migrationView appState model) (ActionResult.combine model.migration model.metrics)


migrationView : AppState -> Model -> ( Migration, List Metric ) -> Html Msg
migrationView appState model ( migration, metrics ) =
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
                                |> Maybe.map (getEventView appState model migration metrics)
                                |> Maybe.map (List.singleton >> div [ class "col-8" ])
                                |> Maybe.withDefault (div [ class "col-12" ] [ errorMessage ])

                        diffTree =
                            migration.migrationState.targetEvent
                                |> Maybe.map (DiffTree.view appState migration.currentKnowledgeModel)
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
    div [ class "col KMEditor__Migration" ]
        [ div [] [ Page.header (lg "kmMigration" appState) [] ]
        , FormResult.view model.conflict
        , currentView
        ]


migrationSummary : AppState -> Migration -> Html Msg
migrationSummary appState migration =
    div [ class "col-12" ]
        [ p []
            (lh_ "summary"
                [ strong [] [ text migration.currentKnowledgeModel.name ]
                , code [] [ text migration.branchPreviousPackageId ]
                , code [] [ text migration.targetPackageId ]
                ]
                appState
            )
        ]


getEventView : AppState -> Model -> Migration -> List Metric -> Event -> Html Msg
getEventView appState model migration metrics event =
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
                |> viewEvent appState model (lg "event.editKM" appState)

        AddTagEvent eventData _ ->
            viewAddTagDiff appState eventData
                |> viewEvent appState model (lg "event.addTag" appState)

        EditTagEvent eventData commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditTagDiff appState eventData)
                |> Maybe.map (viewEvent appState model (lg "event.editTag" appState))
                |> Maybe.withDefault errorMessage

        DeleteTagEvent commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteTagDiff appState)
                |> Maybe.map (viewEvent appState model (lg "event.deleteTag" appState))
                |> Maybe.withDefault errorMessage

        AddIntegrationEvent eventData _ ->
            viewAddIntegrationDiff appState eventData
                |> viewEvent appState model (lg "event.addIntegration" appState)

        EditIntegrationEvent eventData commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditIntegrationDiff appState eventData)
                |> Maybe.map (viewEvent appState model (lg "event.editIntegration" appState))
                |> Maybe.withDefault errorMessage

        DeleteIntegrationEvent commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteIntegrationDiff appState)
                |> Maybe.map (viewEvent appState model (lg "event.deleteIntegration" appState))
                |> Maybe.withDefault errorMessage

        AddChapterEvent eventData _ ->
            viewAddChapterDiff appState eventData
                |> viewEvent appState model (lg "event.addChapter" appState)

        EditChapterEvent eventData commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditChapterDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model (lg "event.editChapter" appState))
                |> Maybe.withDefault errorMessage

        DeleteChapterEvent commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteChapterDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model (lg "event.deleteChapter" appState))
                |> Maybe.withDefault errorMessage

        AddQuestionEvent eventData _ ->
            viewAddQuestionDiff appState migration.currentKnowledgeModel eventData
                |> viewEvent appState model (lg "event.addQuestion" appState)

        EditQuestionEvent eventData commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditQuestionDiff appState migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent appState model (lg "event.editQuestion" appState))
                |> Maybe.withDefault errorMessage

        DeleteQuestionEvent commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteQuestionDiff appState migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent appState model (lg "event.deleteQuestion" appState))
                |> Maybe.withDefault errorMessage

        AddAnswerEvent eventData _ ->
            viewAddAnswerDiff appState metrics eventData
                |> viewEvent appState model (lg "event.addAnswer" appState)

        EditAnswerEvent eventData commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditAnswerDiff appState migration.currentKnowledgeModel metrics eventData)
                |> Maybe.map (viewEvent appState model (lg "event.editAnswer" appState))
                |> Maybe.withDefault errorMessage

        DeleteAnswerEvent commonData ->
            KnowledgeModel.getAnswer commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteAnswerDiff appState migration.currentKnowledgeModel metrics)
                |> Maybe.map (viewEvent appState model (lg "event.deleteAnswer" appState))
                |> Maybe.withDefault errorMessage

        AddReferenceEvent eventData _ ->
            viewAddReferenceDiff appState eventData
                |> viewEvent appState model (lg "event.addReference" appState)

        EditReferenceEvent eventData commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditReferenceDiff appState eventData)
                |> Maybe.map (viewEvent appState model (lg "event.editReference" appState))
                |> Maybe.withDefault errorMessage

        DeleteReferenceEvent commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteReferenceDiff appState)
                |> Maybe.map (viewEvent appState model (lg "event.deleteReference" appState))
                |> Maybe.withDefault errorMessage

        AddExpertEvent eventData _ ->
            viewAddExpertDiff appState eventData
                |> viewEvent appState model (lg "event.addExpert" appState)

        EditExpertEvent eventData commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditExpertDiff appState eventData)
                |> Maybe.map (viewEvent appState model (lg "event.editExpert" appState))
                |> Maybe.withDefault errorMessage

        DeleteExpertEvent commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteExpertDiff appState)
                |> Maybe.map (viewEvent appState model (lg "event.deleteExpert" appState))
                |> Maybe.withDefault errorMessage


viewEvent : AppState -> Model -> String -> Html Msg -> Html Msg
viewEvent appState model name diffView =
    div []
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
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ lg "knowledgeModel.name" appState ]
                    [ km.name ]
                    [ EventField.getValueWithDefault event.name km.name ]

        chapters =
            KnowledgeModel.getChapters km

        originalChapters =
            List.map .uuid chapters

        chapterNames =
            Dict.fromList <| List.map (\c -> ( c.uuid, c.title )) chapters

        chaptersDiff =
            viewDiffChildren (lg "chapters" appState) originalChapters (EventField.getValueWithDefault event.chapterUuids originalChapters) chapterNames

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
    in
    div []
        (fieldDiff ++ [ chaptersDiff, tagsDiff, integrationsDiff ])


viewAddTagDiff : AppState -> AddTagEventData -> Html Msg
viewAddTagDiff appState event =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ lg "tag.name" appState
                , lg "tag.description" appState
                , lg "tag.color" appState
                ]
                [ event.name
                , event.description |> Maybe.withDefault ""
                , event.color
                ]
    in
    div []
        (viewAdd fields)


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
    in
    div [] fieldDiff


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
    in
    div [] fieldDiff


viewAddIntegrationDiff : AppState -> AddIntegrationEventData -> Html Msg
viewAddIntegrationDiff appState event =
    let
        fieldDiff =
            viewAdd <|
                List.map2 (\a b -> ( a, b ))
                    [ lg "integration.id" appState
                    , lg "integration.name" appState
                    , lg "integration.props" appState
                    , lg "integration.itemUrl" appState
                    , lg "integration.request.method" appState
                    , lg "integration.request.url" appState
                    , lg "integration.request.headers" appState
                    , lg "integration.request.body" appState
                    , lg "integration.response.listField" appState
                    , lg "integration.response.idField" appState
                    , lg "integration.response.nameField" appState
                    ]
                    [ event.id
                    , event.name
                    , String.join ", " event.props
                    , event.itemUrl
                    , event.requestMethod
                    , event.requestUrl
                    , String.join ", " <| List.map (\( h, v ) -> h ++ ": " ++ v) <| Dict.toList event.requestHeaders
                    , event.requestBody
                    , event.responseListField
                    , event.responseIdField
                    , event.responseNameField
                    ]
    in
    div [] fieldDiff


viewEditIntegrationDiff : AppState -> EditIntegrationEventData -> Integration -> Html Msg
viewEditIntegrationDiff appState event integration =
    let
        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ lg "integration.id" appState
                    , lg "integration.name" appState
                    , lg "integration.props" appState
                    , lg "integration.itemUrl" appState
                    , lg "integration.request.method" appState
                    , lg "integration.request.url" appState
                    , lg "integration.request.headers" appState
                    , lg "integration.request.body" appState
                    , lg "integration.response.listField" appState
                    , lg "integration.response.idField" appState
                    , lg "integration.response.nameField" appState
                    ]
                    [ integration.id
                    , integration.name
                    , String.join ", " integration.props
                    , integration.itemUrl
                    , integration.requestMethod
                    , integration.requestUrl
                    , String.join ", " <| List.map (\( h, v ) -> h ++ ": " ++ v) <| Dict.toList integration.requestHeaders
                    , integration.requestBody
                    , integration.responseListField
                    , integration.responseIdField
                    , integration.responseNameField
                    ]
                    [ EventField.getValueWithDefault event.id integration.id
                    , EventField.getValueWithDefault event.name integration.name
                    , String.join ", " <| EventField.getValueWithDefault event.props integration.props
                    , EventField.getValueWithDefault event.itemUrl integration.itemUrl
                    , EventField.getValueWithDefault event.requestMethod integration.requestMethod
                    , EventField.getValueWithDefault event.requestUrl integration.requestUrl
                    , String.join ", " <| List.map (\( h, v ) -> h ++ ": " ++ v) <| Dict.toList <| EventField.getValueWithDefault event.requestHeaders integration.requestHeaders
                    , EventField.getValueWithDefault event.requestBody integration.requestBody
                    , EventField.getValueWithDefault event.responseListField integration.responseListField
                    , EventField.getValueWithDefault event.responseIdField integration.responseIdField
                    , EventField.getValueWithDefault event.responseNameField integration.responseNameField
                    ]
    in
    div [] fieldDiff


viewDeleteIntegrationDiff : AppState -> Integration -> Html Msg
viewDeleteIntegrationDiff appState integration =
    let
        fieldDiff =
            viewDelete <|
                List.map2 (\a b -> ( a, b ))
                    [ lg "integration.id" appState
                    , lg "integration.name" appState
                    , lg "integration.props" appState
                    , lg "integration.itemUrl" appState
                    , lg "integration.request.method" appState
                    , lg "integration.request.url" appState
                    , lg "integration.request.headers" appState
                    , lg "integration.request.body" appState
                    , lg "integration.response.listField" appState
                    , lg "integration.response.idField" appState
                    , lg "integration.response.nameField" appState
                    ]
                    [ integration.id
                    , integration.name
                    , String.join ", " integration.props
                    , integration.itemUrl
                    , integration.requestMethod
                    , integration.requestUrl
                    , String.join ", " <| List.map (\( h, v ) -> h ++ ": " ++ v) <| Dict.toList integration.requestHeaders
                    , integration.requestBody
                    , integration.responseListField
                    , integration.responseIdField
                    , integration.responseNameField
                    ]
    in
    div [] fieldDiff


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
    in
    div [] fieldDiff


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
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


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
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


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
                , AddQuestionEventQuestion.map .title .title .title .title event
                , AddQuestionEventQuestion.map .text .text .text .text event |> Maybe.withDefault ""
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
            AddQuestionEventQuestion.map .tagUuids .tagUuids .tagUuids .tagUuids event

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        originalTags =
            List.map (\_ -> "") tagUuids

        tagsDiff =
            viewDiffChildren (lg "tags" appState) originalTags tagUuids tagNames
    in
    div []
        (fieldsDiff ++ [ integrationPropsDiff, tagsDiff ])


viewEditQuestionDiff : AppState -> KnowledgeModel -> EditQuestionEventData -> Question -> Html Msg
viewEditQuestionDiff appState km event question =
    let
        -- Fields
        questionUuid =
            Question.getUuid question

        title =
            EditQuestionEventData.map .title .title .title .title event

        questionText =
            EditQuestionEventData.map .text .text .text .text event

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
                (EventField.getValueWithDefault (EditQuestionEventData.map .tagUuids .tagUuids .tagUuids .tagUuids event) originalTags)
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
                (EventField.getValueWithDefault (EditQuestionEventData.map .referenceUuids .referenceUuids .referenceUuids .referenceUuids event) originalReferences)
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
                (EventField.getValueWithDefault (EditQuestionEventData.map .expertUuids .expertUuids .expertUuids .expertUuids event) originalExperts)
                expertNames
    in
    div []
        (fieldDiff ++ [ integrationPropsDiff, tagsDiff, answersDiff, itemTemplateQuestionsDiff, referencesDiff, expertsDiff ])


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
    in
    div []
        (fieldDiff ++ [ tagsDiff, answersDiff, itemTemplateQuestionsDiff, referencesDiff, expertsDiff ])


getIntegrationName : KnowledgeModel -> String -> String
getIntegrationName km integrationUuid =
    KnowledgeModel.getIntegration integrationUuid km
        |> Maybe.map .name
        |> Maybe.withDefault ""


viewAddAnswerDiff : AppState -> List Metric -> AddAnswerEventData -> Html Msg
viewAddAnswerDiff appState metrics event =
    let
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
    in
    div [] (fieldsDiff ++ [ metricsDiff ])


viewEditAnswerDiff : AppState -> KnowledgeModel -> List Metric -> EditAnswerEventData -> Answer -> Html Msg
viewEditAnswerDiff appState km metrics event answer =
    let
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
    in
    div []
        (fieldDiff ++ [ questionsDiff, metricsPropsDiff ])


viewDeleteAnswerDiff : AppState -> KnowledgeModel -> List Metric -> Answer -> Html Msg
viewDeleteAnswerDiff appState km metrics answer =
    let
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
    in
    div []
        (fieldDiff ++ [ questionsDiff, metricsDiff ])


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


viewAddReferenceDiff : AppState -> AddReferenceEventData -> Html Msg
viewAddReferenceDiff appState =
    AddReferenceEventData.map
        (viewAddResourcePageReferenceDiff appState)
        (viewAddURLReferenceDiff appState)
        (viewAddCrossReferenceDiff appState)


viewAddResourcePageReferenceDiff : AppState -> AddReferenceResourcePageEventData -> Html Msg
viewAddResourcePageReferenceDiff appState data =
    div [] <|
        viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ lg "referenceType" appState
                , lg "reference.shortUuid" appState
                ]
                [ lg "referenceType.resourcePage" appState
                , data.shortUuid
                ]


viewAddURLReferenceDiff : AppState -> AddReferenceURLEventData -> Html Msg
viewAddURLReferenceDiff appState data =
    div [] <|
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


viewAddCrossReferenceDiff : AppState -> AddReferenceCrossEventData -> Html Msg
viewAddCrossReferenceDiff appState data =
    div [] <|
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


viewEditReferenceDiff : AppState -> EditReferenceEventData -> Reference -> Html Msg
viewEditReferenceDiff appState event reference =
    case ( event, reference ) of
        ( EditReferenceResourcePageEvent eventData, ResourcePageReference referenceData ) ->
            div []
                (viewDiff <|
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
                )

        ( EditReferenceURLEvent eventData, URLReference referenceData ) ->
            div []
                (viewDiff <|
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
                )

        ( EditReferenceCrossEvent eventData, CrossReference referenceData ) ->
            div []
                (viewDiff <|
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
                )

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
    div [] <|
        viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ lg "referenceType" appState
                , lg "reference.shortUuid" appState
                ]
                [ lg "referenceType.resourcePage" appState
                , EventField.getValueWithDefault data.shortUuid ""
                ]


viewEditURLReferenceDiff : AppState -> EditReferenceURLEventData -> Html Msg
viewEditURLReferenceDiff appState data =
    div [] <|
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


viewEditCrossReferenceDiff : AppState -> EditReferenceCrossEventData -> Html Msg
viewEditCrossReferenceDiff appState data =
    div [] <|
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


viewDeleteReferenceDiff : AppState -> Reference -> Html Msg
viewDeleteReferenceDiff appState =
    Reference.map
        (viewDeleteResourcePageReferenceDiff appState)
        (viewDeleteURLReferenceDiff appState)
        (viewDeleteCrossReferenceDiff appState)


viewDeleteResourcePageReferenceDiff : AppState -> ResourcePageReferenceData -> Html Msg
viewDeleteResourcePageReferenceDiff appState data =
    div [] <|
        viewDelete <|
            List.map2 (\a b -> ( a, b ))
                [ lg "referenceType" appState
                , lg "reference.shortUuid" appState
                ]
                [ lg "referenceType.resourcePage" appState
                , data.shortUuid
                ]


viewDeleteURLReferenceDiff : AppState -> URLReferenceData -> Html Msg
viewDeleteURLReferenceDiff appState data =
    div [] <|
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


viewDeleteCrossReferenceDiff : AppState -> CrossReferenceData -> Html Msg
viewDeleteCrossReferenceDiff appState data =
    div [] <|
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


viewAddExpertDiff : AppState -> AddExpertEventData -> Html Msg
viewAddExpertDiff appState event =
    div [] <|
        viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ lg "expert.name" appState
                , lg "expert.email" appState
                ]
                [ event.name
                , event.email
                ]


viewEditExpertDiff : AppState -> EditExpertEventData -> Expert -> Html Msg
viewEditExpertDiff appState event expert =
    div [] <|
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


viewDeleteExpertDiff : AppState -> Expert -> Html Msg
viewDeleteExpertDiff appState expert =
    div [] <|
        viewDelete <|
            List.map2 (\a b -> ( a, b ))
                [ lg "expert.name" appState
                , lg "expert.email" appState
                ]
                [ expert.name
                , expert.email
                ]


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
        [ button [ class "btn btn-warning", onClick RejectEvent, disabled actionsDisabled ]
            [ lx_ "action.reject" appState ]
        , button [ class "btn btn-success", onClick ApplyEvent, disabled actionsDisabled ]
            [ lx_ "action.apply" appState ]
        ]


viewCompletedMigration : AppState -> Model -> Html Msg
viewCompletedMigration appState model =
    div [ class "col-xs-12" ]
        [ div [ class "jumbotron full-page-error" ]
            [ h1 [ class "display-3" ] [ i [ class "fa fa-check-square-o" ] [] ]
            , p []
                [ lx_ "completed.msg1" appState
                , br [] []
                , lx_ "completed.msg2" appState
                ]
            , div [ class "text-right" ]
                [ linkTo appState
                    (Routes.KMEditorRoute <| PublishRoute model.branchUuid)
                    [ class "btn btn-primary" ]
                    [ lx_ "completed.publish" appState
                    , i [ class "fa fa-long-arrow-right", style "margin-left" "10px" ] []
                    ]
                ]
            ]
        ]
