module KMEditor.Migration.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (..)
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
import KMEditor.Common.KnowledgeModel.Question as Question exposing (Question(..))
import KMEditor.Common.KnowledgeModel.Question.QuestionValueType as QuestionValueType
import KMEditor.Common.KnowledgeModel.Reference as Reference exposing (Reference(..))
import KMEditor.Common.KnowledgeModel.Reference.CrossReferenceData exposing (CrossReferenceData)
import KMEditor.Common.KnowledgeModel.Reference.ResourcePageReferenceData exposing (ResourcePageReferenceData)
import KMEditor.Common.KnowledgeModel.Reference.URLReferenceData exposing (URLReferenceData)
import KMEditor.Common.KnowledgeModel.Tag exposing (Tag)
import KMEditor.Common.Migration exposing (Migration)
import KMEditor.Common.MigrationStateType exposing (MigrationStateType(..))
import KMEditor.Common.View exposing (diffTreeView)
import KMEditor.Migration.Models exposing (Model)
import KMEditor.Migration.Msgs exposing (Msg(..))
import KMEditor.Routing exposing (Route(..))
import Msgs
import Routing exposing (Route(..))


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (migrationView wrapMsg model) model.migration


migrationView : (Msg -> Msgs.Msg) -> Model -> Migration -> Html Msgs.Msg
migrationView wrapMsg model migration =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text "Migration appState is corrupted." ]

        runningStateMessage =
            div [ class "alert alert-warning" ]
                [ text "Migration is still running, try again later." ]

        currentView =
            case migration.migrationState.stateType of
                ConflictState ->
                    let
                        conflictView =
                            migration.migrationState.targetEvent
                                |> Maybe.map (getEventView wrapMsg model migration)
                                |> Maybe.map (List.singleton >> div [ class "col-8" ])
                                |> Maybe.withDefault (div [ class "col-12" ] [ errorMessage ])

                        diffTree =
                            migration.migrationState.targetEvent
                                |> Maybe.map (List.singleton >> diffTreeView migration.currentKnowledgeModel)
                                |> Maybe.map (List.singleton >> div [ class "col-4" ])
                                |> Maybe.withDefault emptyNode
                    in
                    div [ class "row" ]
                        [ migrationSummary migration, conflictView, diffTree ]

                CompletedState ->
                    viewCompletedMigration model

                RunningState ->
                    runningStateMessage

                _ ->
                    errorMessage
    in
    div [ class "col KMEditor__Migration" ]
        [ div [] [ Page.header "Migration" [] ]
        , FormResult.view model.conflict
        , currentView
        ]


migrationSummary : Migration -> Html Msgs.Msg
migrationSummary migration =
    div [ class "col-12" ]
        [ p []
            [ text "Migration of "
            , strong [] [ text migration.currentKnowledgeModel.name ]
            , text " from "
            , code [] [ text migration.branchPreviousPackageId ]
            , text " to "
            , code [] [ text migration.targetPackageId ]
            , text "."
            ]
        ]


getEventView : (Msg -> Msgs.Msg) -> Model -> Migration -> Event -> Html Msgs.Msg
getEventView wrapMsg model migration event =
    let
        errorMessage =
            div [ class "alert alert-danger" ]
                [ text "The event is not connected to any entity in the knowledge model." ]
    in
    case event of
        AddKnowledgeModelEvent _ _ ->
            -- AddKnowledgeModelEvent should never appear in migrations
            emptyNode

        EditKnowledgeModelEvent eventData _ ->
            migration.currentKnowledgeModel
                |> viewEditKnowledgeModelDiff eventData
                |> viewEvent wrapMsg model "Edit knowledge model"

        AddTagEvent eventData _ ->
            viewAddTagDiff eventData
                |> viewEvent wrapMsg model "Add tag"

        EditTagEvent eventData commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditTagDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit tag")
                |> Maybe.withDefault errorMessage

        DeleteTagEvent commonData ->
            KnowledgeModel.getTag commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map viewDeleteTagDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete tag")
                |> Maybe.withDefault errorMessage

        AddIntegrationEvent eventData _ ->
            viewAddIntegrationDiff eventData
                |> viewEvent wrapMsg model "Add integration"

        EditIntegrationEvent eventData commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditIntegrationDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit integration")
                |> Maybe.withDefault errorMessage

        DeleteIntegrationEvent commonData ->
            KnowledgeModel.getIntegration commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map viewDeleteIntegrationDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete integration")
                |> Maybe.withDefault errorMessage

        AddChapterEvent eventData _ ->
            viewAddChapterDiff eventData
                |> viewEvent wrapMsg model "Add chapter"

        EditChapterEvent eventData commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditChapterDiff migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit chapter")
                |> Maybe.withDefault errorMessage

        DeleteChapterEvent commonData ->
            KnowledgeModel.getChapter commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteChapterDiff migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent wrapMsg model "Delete chapter")
                |> Maybe.withDefault errorMessage

        AddQuestionEvent eventData _ ->
            viewAddQuestionDiff migration.currentKnowledgeModel eventData
                |> viewEvent wrapMsg model "Add question"

        EditQuestionEvent eventData commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditQuestionDiff migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit question")
                |> Maybe.withDefault errorMessage

        DeleteQuestionEvent commonData ->
            KnowledgeModel.getQuestion commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteQuestionDiff migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent wrapMsg model "Delete question")
                |> Maybe.withDefault errorMessage

        AddAnswerEvent eventData _ ->
            viewAddAnswerDiff eventData
                |> viewEvent wrapMsg model "Add answer"

        EditAnswerEvent eventData commonData ->
            KnowledgeModel.getAnswer commonData.uuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditAnswerDiff migration.currentKnowledgeModel eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit answer")
                |> Maybe.withDefault errorMessage

        DeleteAnswerEvent commonData ->
            KnowledgeModel.getAnswer commonData.uuid migration.currentKnowledgeModel
                |> Maybe.map (viewDeleteAnswerDiff migration.currentKnowledgeModel)
                |> Maybe.map (viewEvent wrapMsg model "Delete answer")
                |> Maybe.withDefault errorMessage

        AddReferenceEvent eventData _ ->
            viewAddReferenceDiff eventData
                |> viewEvent wrapMsg model "Add reference"

        EditReferenceEvent eventData commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditReferenceDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit reference")
                |> Maybe.withDefault errorMessage

        DeleteReferenceEvent commonData ->
            KnowledgeModel.getReference commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map viewDeleteReferenceDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete reference")
                |> Maybe.withDefault errorMessage

        AddExpertEvent eventData _ ->
            viewAddExpertDiff eventData
                |> viewEvent wrapMsg model "Add expert"

        EditExpertEvent eventData commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map (viewEditExpertDiff eventData)
                |> Maybe.map (viewEvent wrapMsg model "Edit expert")
                |> Maybe.withDefault errorMessage

        DeleteExpertEvent commonData ->
            KnowledgeModel.getExpert commonData.entityUuid migration.currentKnowledgeModel
                |> Maybe.map viewDeleteExpertDiff
                |> Maybe.map (viewEvent wrapMsg model "Delete expert")
                |> Maybe.withDefault errorMessage


viewEvent : (Msg -> Msgs.Msg) -> Model -> String -> Html Msgs.Msg -> Html Msgs.Msg
viewEvent wrapMsg model name diffView =
    div []
        [ h3 [] [ text name ]
        , div [ class "card bg-light" ]
            [ div [ class "card-body" ]
                [ diffView
                , formActions wrapMsg model
                ]
            ]
        ]


viewEditKnowledgeModelDiff : EditKnowledgeModelEventData -> KnowledgeModel -> Html Msgs.Msg
viewEditKnowledgeModelDiff event km =
    let
        chapters =
            KnowledgeModel.getChapters km

        tags =
            KnowledgeModel.getTags km

        originalChapters =
            List.map .uuid chapters

        chapterNames =
            Dict.fromList <| List.map (\c -> ( c.uuid, c.title )) chapters

        fieldDiff =
            viewDiff <| List.map3 (\a b c -> ( a, b, c )) [ "Name" ] [ km.name ] [ EventField.getValueWithDefault event.name km.name ]

        chaptersDiff =
            viewDiffChildren "Chapters" originalChapters (EventField.getValueWithDefault event.chapterUuids originalChapters) chapterNames

        originalTags =
            List.map .uuid tags

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        tagsDiff =
            viewDiffChildren "Tags" originalTags (EventField.getValueWithDefault event.tagUuids originalTags) tagNames
    in
    div []
        (fieldDiff ++ [ chaptersDiff, tagsDiff ])


viewAddTagDiff : AddTagEventData -> Html Msgs.Msg
viewAddTagDiff event =
    let
        fields =
            List.map2 (\a b -> ( a, b )) [ "Name", "Description", "Color" ] [ event.name, event.description |> Maybe.withDefault "", event.color ]
    in
    div []
        (viewAdd fields)


viewEditTagDiff : EditTagEventData -> Tag -> Html Msgs.Msg
viewEditTagDiff event tag =
    let
        fieldDiff =
            List.map3 (\a b c -> ( a, b, c ))
                [ "Name", "Description", "Color" ]
                [ tag.name, Maybe.withDefault "" tag.description, tag.color ]
                [ EventField.getValueWithDefault event.name tag.name
                , EventField.getValueWithDefault event.description tag.description |> Maybe.withDefault ""
                , EventField.getValueWithDefault event.color tag.color
                ]
    in
    div []
        (viewDiff fieldDiff)


viewDeleteTagDiff : Tag -> Html Msgs.Msg
viewDeleteTagDiff tag =
    let
        fieldDiff =
            List.map2 (\a b -> ( a, b )) [ "Name", "Description", "Color" ] [ tag.name, tag.description |> Maybe.withDefault "", tag.color ]
    in
    div []
        (viewDelete fieldDiff)


viewAddIntegrationDiff : AddIntegrationEventData -> Html Msgs.Msg
viewAddIntegrationDiff event =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ "Id"
                , "Name"
                , "Props"
                , "Item URL"
                , "Request Method"
                , "Request URL"
                , "Request Headers"
                , "Request Body"
                , "Response List Field"
                , "Response Id Field"
                , "Response Name Field"
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
    div []
        (viewAdd fields)


viewEditIntegrationDiff : EditIntegrationEventData -> Integration -> Html Msgs.Msg
viewEditIntegrationDiff event integration =
    let
        fieldDiff =
            List.map3 (\a b c -> ( a, b, c ))
                [ "Id"
                , "Name"
                , "Props"
                , "Item URL"
                , "Request Method"
                , "Request URL"
                , "Request Headers"
                , "Request Body"
                , "Response List Field"
                , "Response Id Field"
                , "Response Name Field"
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
    div []
        (viewDiff fieldDiff)


viewDeleteIntegrationDiff : Integration -> Html Msgs.Msg
viewDeleteIntegrationDiff integration =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ "Id"
                , "Name"
                , "Props"
                , "Item URL"
                , "Request Method"
                , "Request URL"
                , "Request Headers"
                , "Request Body"
                , "Response List Field"
                , "Response Id Field"
                , "Response Name Field"
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
    div []
        (viewDelete fields)


viewAddChapterDiff : AddChapterEventData -> Html Msgs.Msg
viewAddChapterDiff event =
    let
        fields =
            List.map2 (\a b -> ( a, b )) [ "Title", "Text" ] [ event.title, event.text ]
    in
    div []
        (viewAdd fields)


viewEditChapterDiff : KnowledgeModel -> EditChapterEventData -> Chapter -> Html Msgs.Msg
viewEditChapterDiff km event chapter =
    let
        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km

        originalQuestions =
            List.map Question.getUuid questions

        questionNames =
            Dict.fromList <| List.map (\q -> ( Question.getUuid q, Question.getTitle q )) questions

        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ "Title", "Text" ]
                    [ chapter.title, chapter.text ]
                    [ EventField.getValueWithDefault event.title chapter.title
                    , EventField.getValueWithDefault event.text chapter.text
                    ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions (EventField.getValueWithDefault event.questionUuids originalQuestions) questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewDeleteChapterDiff : KnowledgeModel -> Chapter -> Html Msgs.Msg
viewDeleteChapterDiff km chapter =
    let
        questions =
            KnowledgeModel.getChapterQuestions chapter.uuid km

        questionNames =
            List.map Question.getTitle questions

        fieldDiff =
            viewDelete <| List.map2 (\a b -> ( a, b )) [ "Title", "Text" ] [ chapter.title, chapter.text ]

        questionsDiff =
            viewDeletedChildren "Questions" questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewAddQuestionDiff : KnowledgeModel -> AddQuestionEventData -> Html Msgs.Msg
viewAddQuestionDiff km event =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ "Type", "Title", "Text", "Tags" ]
                [ AddQuestionEventQuestion.getTypeString event
                , AddQuestionEventQuestion.map .title .title .title .title event
                , AddQuestionEventQuestion.map .text .text .text .text event |> Maybe.withDefault ""
                ]

        extraFields =
            case event of
                AddQuestionValueEvent data ->
                    [ ( "Value Type", QuestionValueType.toString data.valueType ) ]

                AddQuestionIntegrationEvent data ->
                    [ ( "Integration"
                      , KnowledgeModel.getIntegration data.integrationUuid km
                            |> Maybe.map .name
                            |> Maybe.withDefault ""
                      )
                    ]

                _ ->
                    []

        tags =
            KnowledgeModel.getTags km

        tagUuids =
            AddQuestionEventQuestion.map .tagUuids .tagUuids .tagUuids .tagUuids event

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        originalTags =
            List.map (\_ -> "") tagUuids

        tagsDiff =
            viewDiffChildren "Tags" originalTags tagUuids tagNames
    in
    div []
        (viewAdd (fields ++ extraFields) ++ [ tagsDiff ])


viewEditQuestionDiff : KnowledgeModel -> EditQuestionEventData -> Question -> Html Msgs.Msg
viewEditQuestionDiff km event question =
    let
        title =
            EditQuestionEventData.map .title .title .title .title event

        questionText =
            EditQuestionEventData.map .text .text .text .text event

        questionUuid =
            Question.getUuid question

        answers =
            KnowledgeModel.getQuestionAnswers questionUuid km

        originalAnswers =
            List.map .uuid answers

        answerNames =
            Dict.fromList <| List.map (\a -> ( a.uuid, a.label )) answers

        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        originalReferences =
            List.map Reference.getUuid references

        referenceUuids =
            EditQuestionEventData.map .referenceUuids .referenceUuids .referenceUuids .referenceUuids event

        referenceNames =
            Dict.fromList <| List.map (\r -> ( Reference.getUuid r, Reference.getVisibleName r )) references

        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        originalExperts =
            List.map .uuid experts

        expertUuids =
            EditQuestionEventData.map .expertUuids .expertUuids .expertUuids .expertUuids event

        expertNames =
            Dict.fromList <| List.map (\e -> ( e.uuid, e.name )) experts

        fields =
            List.map3 (\a b c -> ( a, b, c ))
                [ "Type", "Title", "Text" ]
                [ Question.getTypeString question
                , Question.getTitle question
                , Question.getText question |> Maybe.withDefault ""
                ]
                [ EditQuestionEventData.getTypeString event
                , EventField.getValueWithDefault title <| Question.getTitle question
                , EventField.getValueWithDefault questionText (Question.getText question) |> Maybe.withDefault ""
                ]

        originalValueType =
            Question.getValueType question

        valueType =
            EditQuestionEventData.map
                (\_ -> Nothing)
                (\_ -> Nothing)
                (\data -> EventField.getValue data.valueType)
                (\_ -> Nothing)
                event

        valueDiff =
            case ( originalValueType, valueType ) of
                ( Nothing, Nothing ) ->
                    []

                ( original, new ) ->
                    let
                        originalStr =
                            Maybe.withDefault "" <| Maybe.map QuestionValueType.toString original

                        newStr =
                            Maybe.withDefault originalStr <| Maybe.map QuestionValueType.toString new
                    in
                    [ ( "Value Type", originalStr, newStr ) ]

        fieldDiff =
            viewDiff (fields ++ valueDiff)

        originalTags =
            Question.getTagUuids question

        tagUuids =
            EventField.getValueWithDefault (EditQuestionEventData.map .tagUuids .tagUuids .tagUuids .tagUuids event) originalTags

        tags =
            KnowledgeModel.getTags km

        tagNames =
            Dict.fromList <| List.map (\t -> ( t.uuid, t.name )) tags

        tagsDiff =
            viewDiffChildren "Tags" originalTags tagUuids tagNames

        answersDiff =
            case event of
                EditQuestionOptionsEvent eventData ->
                    let
                        answerUuids =
                            EventField.getValueWithDefault eventData.answerUuids originalAnswers
                    in
                    viewDiffChildren "Answers" originalAnswers answerUuids answerNames

                _ ->
                    emptyNode

        referencesDiff =
            viewDiffChildren "References" originalReferences (EventField.getValueWithDefault referenceUuids originalReferences) referenceNames

        expertsDiff =
            viewDiffChildren "Experts" originalExperts (EventField.getValueWithDefault expertUuids originalExperts) expertNames
    in
    div []
        (fieldDiff ++ [ tagsDiff, answersDiff, referencesDiff, expertsDiff ])


viewDeleteQuestionDiff : KnowledgeModel -> Question -> Html Msgs.Msg
viewDeleteQuestionDiff km question =
    let
        fields =
            List.map2 (\a b -> ( a, b ))
                [ "Title", "Text" ]
                [ Question.getTitle question, Question.getText question |> Maybe.withDefault "" ]

        valueField =
            case question of
                ValueQuestion _ data ->
                    [ ( "Value Type", QuestionValueType.toString data.valueType ) ]

                _ ->
                    []

        fieldDiff =
            viewDelete (fields ++ valueField)

        questionUuid =
            Question.getUuid question

        answers =
            KnowledgeModel.getQuestionAnswers questionUuid km

        answersDiff =
            viewDeletedChildren "Answers" <| List.map .label answers

        references =
            KnowledgeModel.getQuestionReferences questionUuid km

        referencesDiff =
            viewDeletedChildren "References" <| List.map Reference.getVisibleName references

        experts =
            KnowledgeModel.getQuestionExperts questionUuid km

        expertsDiff =
            viewDeletedChildren "Experts" <| List.map .name experts
    in
    div []
        (fieldDiff ++ [ answersDiff, referencesDiff, expertsDiff ])


viewAddAnswerDiff : AddAnswerEventData -> Html Msgs.Msg
viewAddAnswerDiff event =
    let
        fields =
            List.map2 (\a b -> ( a, b )) [ "Label", "Advice" ] [ event.label, event.advice |> Maybe.withDefault "" ]
    in
    div []
        (viewAdd fields)


viewEditAnswerDiff : KnowledgeModel -> EditAnswerEventData -> Answer -> Html Msgs.Msg
viewEditAnswerDiff km event answer =
    let
        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        originalQuestions =
            List.map Question.getUuid questions

        questionNames =
            Dict.fromList <| List.map (\q -> ( Question.getUuid q, Question.getTitle q )) questions

        fieldDiff =
            viewDiff <|
                List.map3 (\a b c -> ( a, b, c ))
                    [ "Label", "Advice" ]
                    [ answer.label, answer.advice |> Maybe.withDefault "" ]
                    [ EventField.getValueWithDefault event.label answer.label
                    , EventField.getValueWithDefault event.advice answer.advice |> Maybe.withDefault ""
                    ]

        questionsDiff =
            viewDiffChildren "Questions" originalQuestions (EventField.getValueWithDefault event.followUpUuids originalQuestions) questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewDeleteAnswerDiff : KnowledgeModel -> Answer -> Html Msgs.Msg
viewDeleteAnswerDiff km answer =
    let
        questions =
            KnowledgeModel.getAnswerFollowupQuestions answer.uuid km

        questionNames =
            List.map Question.getTitle questions

        fieldDiff =
            viewDelete <| List.map2 (\a b -> ( a, b )) [ "Label", "Advice" ] [ answer.label, answer.advice |> Maybe.withDefault "" ]

        questionsDiff =
            viewDeletedChildren "Questions" questionNames
    in
    div []
        (fieldDiff ++ [ questionsDiff ])


viewAddReferenceDiff : AddReferenceEventData -> Html Msgs.Msg
viewAddReferenceDiff =
    AddReferenceEventData.map
        viewAddResourcePageReferenceDiff
        viewAddURLReferenceDiff
        viewAddCrossReferenceDiff


viewAddResourcePageReferenceDiff : AddReferenceResourcePageEventData -> Html Msgs.Msg
viewAddResourcePageReferenceDiff data =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Type", "Short UUID" ] [ "Resource Page", data.shortUuid ])


viewAddURLReferenceDiff : AddReferenceURLEventData -> Html Msgs.Msg
viewAddURLReferenceDiff data =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Type", "URL", "Label" ] [ "URL", data.url, data.label ])


viewAddCrossReferenceDiff : AddReferenceCrossEventData -> Html Msgs.Msg
viewAddCrossReferenceDiff data =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Type", "Target UUID", "Description" ] [ "Cross Reference", data.targetUuid, data.description ])


viewEditReferenceDiff : EditReferenceEventData -> Reference -> Html Msgs.Msg
viewEditReferenceDiff event reference =
    case ( event, reference ) of
        ( EditReferenceResourcePageEvent eventData, ResourcePageReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (\a b c -> ( a, b, c ))
                        [ "Type", "Short UUID" ]
                        [ "Resource Page", referenceData.shortUuid ]
                        [ "Resource Page", EventField.getValueWithDefault eventData.shortUuid referenceData.shortUuid ]
                )

        ( EditReferenceURLEvent eventData, URLReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (\a b c -> ( a, b, c ))
                        [ "Type", "URL", "Label" ]
                        [ "URL", referenceData.url, referenceData.label ]
                        [ "URL"
                        , EventField.getValueWithDefault eventData.url referenceData.url
                        , EventField.getValueWithDefault eventData.label referenceData.label
                        ]
                )

        ( EditReferenceCrossEvent eventData, CrossReference referenceData ) ->
            div []
                (viewDiff <|
                    List.map3 (\a b c -> ( a, b, c ))
                        [ "Type", "Target UUID", "Description" ]
                        [ "Cross Reference", referenceData.targetUuid, referenceData.description ]
                        [ "Cross Reference"
                        , EventField.getValueWithDefault eventData.targetUuid referenceData.targetUuid
                        , EventField.getValueWithDefault eventData.description referenceData.description
                        ]
                )

        ( otherEvent, otherReference ) ->
            let
                deleteReference =
                    viewDeleteReferenceDiff otherReference

                addReference =
                    EditReferenceEventData.map
                        viewEditResourcePageReferenceDiff
                        viewEditURLReferenceDiff
                        viewEditCrossReferenceDiff
                        otherEvent
            in
            div [] [ deleteReference, addReference ]


viewEditResourcePageReferenceDiff : EditReferenceResourcePageEventData -> Html Msgs.Msg
viewEditResourcePageReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ "Type", "Short UUID" ]
                [ "Resource Page", EventField.getValueWithDefault data.shortUuid "" ]
        )


viewEditURLReferenceDiff : EditReferenceURLEventData -> Html Msgs.Msg
viewEditURLReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ "Type", "URL", "Label" ]
                [ "URL"
                , EventField.getValueWithDefault data.url ""
                , EventField.getValueWithDefault data.label ""
                ]
        )


viewEditCrossReferenceDiff : EditReferenceCrossEventData -> Html Msgs.Msg
viewEditCrossReferenceDiff data =
    div []
        (viewAdd <|
            List.map2 (\a b -> ( a, b ))
                [ "Type", "Target UUID", "Description" ]
                [ "Cross Reference"
                , EventField.getValueWithDefault data.targetUuid ""
                , EventField.getValueWithDefault data.description ""
                ]
        )


viewDeleteReferenceDiff : Reference -> Html Msgs.Msg
viewDeleteReferenceDiff =
    Reference.map
        viewDeleteResourcePageReferenceDiff
        viewDeleteURLReferenceDiff
        viewDeleteCrossReferenceDiff


viewDeleteResourcePageReferenceDiff : ResourcePageReferenceData -> Html Msgs.Msg
viewDeleteResourcePageReferenceDiff data =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Type", "Short UUID" ] [ "Resource Page", data.shortUuid ])


viewDeleteURLReferenceDiff : URLReferenceData -> Html Msgs.Msg
viewDeleteURLReferenceDiff data =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Type", "URL", "Label" ] [ "URL", data.url, data.label ])


viewDeleteCrossReferenceDiff : CrossReferenceData -> Html Msgs.Msg
viewDeleteCrossReferenceDiff data =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Type", "Target UUID", "Description" ] [ "Cross Reference", data.targetUuid, data.description ])


viewAddExpertDiff : AddExpertEventData -> Html Msgs.Msg
viewAddExpertDiff event =
    div []
        (viewAdd <| List.map2 (\a b -> ( a, b )) [ "Name", "Email" ] [ event.name, event.email ])


viewEditExpertDiff : EditExpertEventData -> Expert -> Html Msgs.Msg
viewEditExpertDiff event expert =
    div []
        (viewDiff <|
            List.map3 (\a b c -> ( a, b, c ))
                [ "Name", "Email" ]
                [ expert.name, expert.email ]
                [ EventField.getValueWithDefault event.name expert.name
                , EventField.getValueWithDefault event.email expert.email
                ]
        )


viewDeleteExpertDiff : Expert -> Html Msgs.Msg
viewDeleteExpertDiff expert =
    div []
        (viewDelete <| List.map2 (\a b -> ( a, b )) [ "Name", "Email" ] [ expert.name, expert.email ])


viewDiff : List ( String, String, String ) -> List (Html Msgs.Msg)
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


viewAdd : List ( String, String ) -> List (Html Msgs.Msg)
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


viewDelete : List ( String, String ) -> List (Html Msgs.Msg)
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


viewDiffChildren : String -> List String -> List String -> Dict String String -> Html Msgs.Msg
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
            if List.length originalOrder == 0 then
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


viewDeletedChildren : String -> List String -> Html Msgs.Msg
viewDeletedChildren fieldName children =
    childrenView fieldName <|
        ul [ class "del" ]
            (List.map (\child -> li [] [ text child ]) children)


childrenView : String -> Html Msgs.Msg -> Html Msgs.Msg
childrenView fieldName diffView =
    div [ class "form-group" ]
        [ label [ class "control-label" ]
            [ text fieldName ]
        , div [ class "form-value" ]
            [ diffView ]
        ]


formActions : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
formActions wrapMsg model =
    let
        actionsDisabled =
            case model.conflict of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "form-actions" ]
        [ button [ class "btn btn-warning", onClick (wrapMsg RejectEvent), disabled actionsDisabled ]
            [ text "Reject" ]
        , button [ class "btn btn-success", onClick (wrapMsg ApplyEvent), disabled actionsDisabled ]
            [ text "Apply" ]
        ]


viewCompletedMigration : Model -> Html Msgs.Msg
viewCompletedMigration model =
    div [ class "col-xs-12" ]
        [ div [ class "jumbotron full-page-error" ]
            [ h1 [ class "display-3" ] [ i [ class "fa fa-check-square-o" ] [] ]
            , p []
                [ text "Migration successfully completed."
                , br [] []
                , text "You can publish the new version now."
                ]
            , div [ class "text-right" ]
                [ linkTo (KMEditor <| PublishRoute model.branchUuid)
                    [ class "btn btn-primary" ]
                    [ text "Publish"
                    , i [ class "fa fa-long-arrow-right", style "margin-left" "10px" ] []
                    ]
                ]
            ]
        ]
