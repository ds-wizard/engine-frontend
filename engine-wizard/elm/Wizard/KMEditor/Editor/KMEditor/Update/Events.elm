module Wizard.KMEditor.Editor.KMEditor.Update.Events exposing
    ( createAddAnswerEvent
    , createAddChapterEvent
    , createAddChoiceEvent
    , createAddExpertEvent
    , createAddIntegrationEvent
    , createAddMetricEvent
    , createAddPhaseEvent
    , createAddQuestionEvent
    , createAddReferenceEvent
    , createAddTagEvent
    , createDeleteAnswerEvent
    , createDeleteChapterEvent
    , createDeleteChoiceEvent
    , createDeleteExpertEvent
    , createDeleteIntegrationEvent
    , createDeleteMetricEvent
    , createDeletePhaseEvent
    , createDeleteQuestionEvent
    , createDeleteReferenceEvent
    , createDeleteTagEvent
    , createEditAnswerEvent
    , createEditChapterEvent
    , createEditChoiceEvent
    , createEditExpertEvent
    , createEditIntegrationEvent
    , createEditKnowledgeModelEvent
    , createEditMetricEvent
    , createEditPhaseEvent
    , createEditQuestionEvent
    , createEditReferenceEvent
    , createEditTagEvent
    , createMoveAnswerEvent
    , createMoveChoiceEvent
    , createMoveExpertEvent
    , createMoveQuestionEvent
    , createMoveReferenceEvent
    )

import Dict
import Random exposing (Seed)
import Shared.Data.Event exposing (Event(..))
import Shared.Data.Event.AddQuestionEventData exposing (AddQuestionEventData(..))
import Shared.Data.Event.AddReferenceEventData exposing (AddReferenceEventData(..))
import Shared.Data.Event.CommonEventData exposing (CommonEventData)
import Shared.Data.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Shared.Data.Event.EditReferenceEventData exposing (EditReferenceEventData(..))
import Shared.Data.Event.EventField as EventField
import Shared.Data.Event.MoveEventData exposing (MoveEventData)
import Shared.Data.KnowledgeModel.Question as Question
import Shared.Data.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Shared.Utils exposing (getUuidString)
import Uuid
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (AnswerEditorData, ChapterEditorData, ChoiceEditorData, ExpertEditorData, IntegrationEditorData, KMEditorData, MetricEditorData, PhaseEditorData, QuestionEditorData, ReferenceEditorData, TagEditorData)
import Wizard.KMEditor.Editor.KMEditor.Models.Forms exposing (AnswerForm, ChapterForm, ChoiceForm, ExpertForm, IntegrationForm, KnowledgeModelForm, MetricForm, PhaseForm, QuestionForm, QuestionFormType(..), ReferenceForm, ReferenceFormType(..), TagForm, getMetricMeasures)


createEditKnowledgeModelEvent : KnowledgeModelForm -> KMEditorData -> Seed -> ( Event, Seed )
createEditKnowledgeModelEvent _ editorData =
    let
        data =
            { chapterUuids = EventField.create editorData.chapters.list editorData.chapters.dirty
            , metricUuids = EventField.create editorData.metrics.list editorData.metrics.dirty
            , phaseUuids = EventField.create editorData.phases.list editorData.phases.dirty
            , tagUuids = EventField.create editorData.tags.list editorData.tags.dirty
            , integrationUuids = EventField.create editorData.integrations.list editorData.integrations.dirty
            }
    in
    createEvent (EditKnowledgeModelEvent data) (Uuid.toString editorData.knowledgeModel.uuid) editorData.parentUuid


createAddChapterEvent : ChapterForm -> ChapterEditorData -> Seed -> ( Event, Seed )
createAddChapterEvent form editorData =
    let
        data =
            { title = form.title
            , text = form.text
            }
    in
    createEvent (AddChapterEvent data) editorData.chapter.uuid editorData.parentUuid


createEditChapterEvent : ChapterForm -> ChapterEditorData -> Seed -> ( Event, Seed )
createEditChapterEvent form editorData =
    let
        data =
            { title = EventField.create form.title (editorData.chapter.title /= form.title)
            , text = EventField.create form.text (editorData.chapter.text /= form.text)
            , questionUuids = EventField.create editorData.questions.list editorData.questions.dirty
            }
    in
    createEvent (EditChapterEvent data) editorData.chapter.uuid editorData.parentUuid


createDeleteChapterEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteChapterEvent =
    createEvent DeleteChapterEvent


createAddMetricEvent : MetricForm -> MetricEditorData -> Seed -> ( Event, Seed )
createAddMetricEvent form editorData =
    let
        data =
            { title = form.title
            , abbreviation = form.abbreviation
            , description = form.description
            }
    in
    createEvent (AddMetricEvent data) editorData.metric.uuid editorData.parentUuid


createEditMetricEvent : MetricForm -> MetricEditorData -> Seed -> ( Event, Seed )
createEditMetricEvent form editorData =
    let
        data =
            { title = EventField.create form.title (editorData.metric.title /= form.title)
            , abbreviation = EventField.create form.abbreviation (editorData.metric.abbreviation /= form.abbreviation)
            , description = EventField.create form.description (editorData.metric.description /= form.description)
            }
    in
    createEvent (EditMetricEvent data) editorData.metric.uuid editorData.parentUuid


createDeleteMetricEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteMetricEvent =
    createEvent DeleteMetricEvent


createAddPhaseEvent : PhaseForm -> PhaseEditorData -> Seed -> ( Event, Seed )
createAddPhaseEvent form editorData =
    let
        data =
            { title = form.title
            , description = form.description
            }
    in
    createEvent (AddPhaseEvent data) editorData.phase.uuid editorData.parentUuid


createEditPhaseEvent : PhaseForm -> PhaseEditorData -> Seed -> ( Event, Seed )
createEditPhaseEvent form editorData =
    let
        data =
            { title = EventField.create form.title (editorData.phase.title /= form.title)
            , description = EventField.create form.description (editorData.phase.description /= form.description)
            }
    in
    createEvent (EditPhaseEvent data) editorData.phase.uuid editorData.parentUuid


createDeletePhaseEvent : String -> String -> Seed -> ( Event, Seed )
createDeletePhaseEvent =
    createEvent DeletePhaseEvent


createAddTagEvent : TagForm -> TagEditorData -> Seed -> ( Event, Seed )
createAddTagEvent form editorData =
    let
        data =
            { name = form.name
            , description = form.description
            , color = form.color
            }
    in
    createEvent (AddTagEvent data) editorData.tag.uuid editorData.parentUuid


createEditTagEvent : TagForm -> TagEditorData -> Seed -> ( Event, Seed )
createEditTagEvent form editorData =
    let
        data =
            { name = EventField.create form.name (editorData.tag.name /= form.name)
            , description = EventField.create form.description (editorData.tag.description /= form.description)
            , color = EventField.create form.color (editorData.tag.color /= form.color)
            }
    in
    createEvent (EditTagEvent data) editorData.tag.uuid editorData.parentUuid


createDeleteTagEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteTagEvent =
    createEvent DeleteTagEvent


createAddIntegrationEvent : IntegrationForm -> IntegrationEditorData -> Seed -> ( Event, Seed )
createAddIntegrationEvent form editorData =
    let
        data =
            { id = form.id
            , name = form.name
            , props = editorData.props.list
            , logo = form.logo
            , requestMethod = form.requestMethod
            , requestUrl = form.requestUrl
            , requestHeaders = Dict.fromList form.requestHeaders
            , requestBody = form.requestBody
            , responseListField = form.responseListField
            , responseIdField = form.responseIdField
            , responseNameField = form.responseNameField
            , itemUrl = form.itemUrl
            }
    in
    createEvent (AddIntegrationEvent data) editorData.integration.uuid editorData.parentUuid


createEditIntegrationEvent : IntegrationForm -> IntegrationEditorData -> Seed -> ( Event, Seed )
createEditIntegrationEvent form editorData =
    let
        requestHeaders =
            Dict.fromList form.requestHeaders

        data =
            { id = EventField.create form.id (editorData.integration.id /= form.id)
            , name = EventField.create form.name (editorData.integration.name /= form.name)
            , props = EventField.create editorData.props.list editorData.props.dirty
            , logo = EventField.create form.logo (editorData.integration.logo /= form.logo)
            , requestMethod = EventField.create form.requestMethod (editorData.integration.requestMethod /= form.requestMethod)
            , requestUrl = EventField.create form.requestUrl (editorData.integration.requestUrl /= form.requestUrl)
            , requestHeaders = EventField.create requestHeaders (editorData.integration.requestHeaders /= requestHeaders)
            , requestBody = EventField.create form.requestBody (editorData.integration.requestBody /= form.requestBody)
            , responseListField = EventField.create form.responseListField (editorData.integration.responseListField /= form.responseListField)
            , responseIdField = EventField.create form.responseIdField (editorData.integration.responseIdField /= form.responseIdField)
            , responseNameField = EventField.create form.responseNameField (editorData.integration.responseNameField /= form.responseNameField)
            , itemUrl = EventField.create form.itemUrl (editorData.integration.itemUrl /= form.itemUrl)
            }
    in
    createEvent (EditIntegrationEvent data) editorData.integration.uuid editorData.parentUuid


createDeleteIntegrationEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteIntegrationEvent =
    createEvent DeleteIntegrationEvent


createAddQuestionEvent : QuestionForm -> QuestionEditorData -> Seed -> ( Event, Seed )
createAddQuestionEvent form editorData =
    let
        data =
            case form.question of
                OptionsQuestionForm formData ->
                    AddQuestionOptionsEvent
                        { title = formData.title
                        , text = formData.text
                        , requiredPhaseUuid = formData.requiredPhase
                        , tagUuids = editorData.tagUuids
                        }

                ListQuestionForm formData ->
                    AddQuestionListEvent
                        { title = formData.title
                        , text = formData.text
                        , requiredPhaseUuid = formData.requiredPhase
                        , tagUuids = editorData.tagUuids
                        }

                ValueQuestionForm formData ->
                    AddQuestionValueEvent
                        { title = formData.title
                        , text = formData.text
                        , requiredPhaseUuid = formData.requiredPhase
                        , tagUuids = editorData.tagUuids
                        , valueType = formData.valueType
                        }

                IntegrationQuestionForm formData ->
                    AddQuestionIntegrationEvent
                        { title = formData.title
                        , text = formData.text
                        , requiredPhaseUuid = formData.requiredPhase
                        , tagUuids = editorData.tagUuids
                        , integrationUuid = formData.integrationUuid
                        , props = formData.props
                        }

                MultiChoiceQuestionForm formData ->
                    AddQuestionMultiChoiceEvent
                        { title = formData.title
                        , text = formData.text
                        , requiredPhaseUuid = formData.requiredPhase
                        , tagUuids = editorData.tagUuids
                        }
    in
    createEvent (AddQuestionEvent data) (Question.getUuid editorData.question) editorData.parentUuid


createEditQuestionEvent : QuestionForm -> QuestionEditorData -> Seed -> ( Event, Seed )
createEditQuestionEvent form editorData =
    let
        data =
            case form.question of
                OptionsQuestionForm formData ->
                    EditQuestionOptionsEvent
                        { title = EventField.create formData.title (Question.getTitle editorData.question /= formData.title)
                        , text = EventField.create formData.text (Question.getText editorData.question /= formData.text)
                        , requiredPhaseUuid = EventField.create formData.requiredPhase (Question.getRequiredPhaseUuid editorData.question /= formData.requiredPhase)
                        , tagUuids = EventField.create editorData.tagUuids (Question.getTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = EventField.create editorData.references.list editorData.references.dirty
                        , expertUuids = EventField.create editorData.experts.list editorData.experts.dirty
                        , answerUuids = EventField.create editorData.answers.list editorData.answers.dirty
                        }

                ListQuestionForm formData ->
                    EditQuestionListEvent
                        { title = EventField.create formData.title (Question.getTitle editorData.question /= formData.title)
                        , text = EventField.create formData.text (Question.getText editorData.question /= formData.text)
                        , requiredPhaseUuid = EventField.create formData.requiredPhase (Question.getRequiredPhaseUuid editorData.question /= formData.requiredPhase)
                        , tagUuids = EventField.create editorData.tagUuids (Question.getTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = EventField.create editorData.references.list editorData.references.dirty
                        , expertUuids = EventField.create editorData.experts.list editorData.experts.dirty
                        , itemTemplateQuestionUuids = EventField.create editorData.itemTemplateQuestions.list editorData.itemTemplateQuestions.dirty
                        }

                ValueQuestionForm formData ->
                    EditQuestionValueEvent
                        { title = EventField.create formData.title (Question.getTitle editorData.question /= formData.title)
                        , text = EventField.create formData.text (Question.getText editorData.question /= formData.text)
                        , requiredPhaseUuid = EventField.create formData.requiredPhase (Question.getRequiredPhaseUuid editorData.question /= formData.requiredPhase)
                        , tagUuids = EventField.create editorData.tagUuids (Question.getTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = EventField.create editorData.references.list editorData.references.dirty
                        , expertUuids = EventField.create editorData.experts.list editorData.experts.dirty
                        , valueType = EventField.create formData.valueType (Question.getValueType editorData.question /= Just formData.valueType)
                        }

                IntegrationQuestionForm formData ->
                    EditQuestionIntegrationEvent
                        { title = EventField.create formData.title (Question.getTitle editorData.question /= formData.title)
                        , text = EventField.create formData.text (Question.getText editorData.question /= formData.text)
                        , requiredPhaseUuid = EventField.create formData.requiredPhase (Question.getRequiredPhaseUuid editorData.question /= formData.requiredPhase)
                        , tagUuids = EventField.create editorData.tagUuids (Question.getTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = EventField.create editorData.references.list editorData.references.dirty
                        , expertUuids = EventField.create editorData.experts.list editorData.experts.dirty
                        , integrationUuid = EventField.create formData.integrationUuid (Question.getIntegrationUuid editorData.question /= Just formData.integrationUuid)
                        , props = EventField.create formData.props (Question.getProps editorData.question /= Just formData.props)
                        }

                MultiChoiceQuestionForm formData ->
                    EditQuestionMultiChoiceEvent
                        { title = EventField.create formData.title (Question.getTitle editorData.question /= formData.title)
                        , text = EventField.create formData.text (Question.getText editorData.question /= formData.text)
                        , requiredPhaseUuid = EventField.create formData.requiredPhase (Question.getRequiredPhaseUuid editorData.question /= formData.requiredPhase)
                        , tagUuids = EventField.create editorData.tagUuids (Question.getTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = EventField.create editorData.references.list editorData.references.dirty
                        , expertUuids = EventField.create editorData.experts.list editorData.experts.dirty
                        , choiceUuids = EventField.create editorData.choices.list editorData.choices.dirty
                        }
    in
    createEvent (EditQuestionEvent data) (Question.getUuid editorData.question) editorData.parentUuid


createDeleteQuestionEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteQuestionEvent =
    createEvent DeleteQuestionEvent


createAddAnswerEvent : AnswerForm -> AnswerEditorData -> Seed -> ( Event, Seed )
createAddAnswerEvent form editorData =
    let
        data =
            { label = form.label
            , advice = form.advice
            , metricMeasures = getMetricMeasures form
            }
    in
    createEvent (AddAnswerEvent data) editorData.answer.uuid editorData.parentUuid


createEditAnswerEvent : AnswerForm -> AnswerEditorData -> Seed -> ( Event, Seed )
createEditAnswerEvent form editorData =
    let
        metricMeasures =
            getMetricMeasures form

        data =
            { label = EventField.create form.label (editorData.answer.label /= form.label)
            , advice = EventField.create form.advice (editorData.answer.advice /= form.advice)
            , metricMeasures = EventField.create metricMeasures (editorData.answer.metricMeasures /= metricMeasures)
            , followUpUuids = EventField.create editorData.followUps.list editorData.followUps.dirty
            }
    in
    createEvent (EditAnswerEvent data) editorData.answer.uuid editorData.parentUuid


createDeleteAnswerEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteAnswerEvent =
    createEvent DeleteAnswerEvent


createAddChoiceEvent : ChoiceForm -> ChoiceEditorData -> Seed -> ( Event, Seed )
createAddChoiceEvent form editorData =
    let
        data =
            { label = form.label
            }
    in
    createEvent (AddChoiceEvent data) editorData.choice.uuid editorData.parentUuid


createEditChoiceEvent : ChoiceForm -> ChoiceEditorData -> Seed -> ( Event, Seed )
createEditChoiceEvent form editorData =
    let
        data =
            { label = EventField.create form.label (editorData.choice.label /= form.label)
            }
    in
    createEvent (EditChoiceEvent data) editorData.choice.uuid editorData.parentUuid


createDeleteChoiceEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteChoiceEvent =
    createEvent DeleteChoiceEvent


createAddReferenceEvent : ReferenceForm -> ReferenceEditorData -> Seed -> ( Event, Seed )
createAddReferenceEvent form editorData =
    let
        data =
            case form.reference of
                ResourcePageReferenceFormType shortUuid ->
                    AddReferenceResourcePageEvent
                        { shortUuid = shortUuid
                        }

                URLReferenceFormType url label ->
                    AddReferenceURLEvent
                        { url = url
                        , label = label
                        }

                CrossReferenceFormType targetUuid description ->
                    AddReferenceCrossEvent
                        { targetUuid = targetUuid
                        , description = description
                        }
    in
    createEvent (AddReferenceEvent data) (Reference.getUuid editorData.reference) editorData.parentUuid


createEditReferenceEvent : ReferenceForm -> ReferenceEditorData -> Seed -> ( Event, Seed )
createEditReferenceEvent form editorData =
    let
        resourcePageEventField field newValue =
            let
                changed =
                    case editorData.reference of
                        ResourcePageReference resourcePageData ->
                            field resourcePageData /= newValue

                        _ ->
                            True
            in
            EventField.create newValue changed

        urlEventField field newValue =
            let
                changed =
                    case editorData.reference of
                        URLReference urlData ->
                            field urlData /= newValue

                        _ ->
                            True
            in
            EventField.create newValue changed

        crossEventField field newValue =
            let
                changed =
                    case editorData.reference of
                        CrossReference crossReferenceData ->
                            field crossReferenceData /= newValue

                        _ ->
                            True
            in
            EventField.create newValue changed

        data =
            case form.reference of
                ResourcePageReferenceFormType shortUuid ->
                    EditReferenceResourcePageEvent
                        { shortUuid = resourcePageEventField .shortUuid shortUuid
                        }

                URLReferenceFormType url label ->
                    EditReferenceURLEvent
                        { url = urlEventField .url url
                        , label = urlEventField .label label
                        }

                CrossReferenceFormType targetUuid description ->
                    EditReferenceCrossEvent
                        { targetUuid = crossEventField .targetUuid targetUuid
                        , description = crossEventField .description description
                        }
    in
    createEvent (EditReferenceEvent data) (Reference.getUuid editorData.reference) editorData.parentUuid


createDeleteReferenceEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteReferenceEvent =
    createEvent DeleteReferenceEvent


createAddExpertEvent : ExpertForm -> ExpertEditorData -> Seed -> ( Event, Seed )
createAddExpertEvent form editorData =
    let
        data =
            { name = form.name
            , email = form.email
            }
    in
    createEvent (AddExpertEvent data) editorData.expert.uuid editorData.parentUuid


createEditExpertEvent : ExpertForm -> ExpertEditorData -> Seed -> ( Event, Seed )
createEditExpertEvent form editorData =
    let
        data =
            { name = EventField.create form.name (editorData.expert.name /= form.name)
            , email = EventField.create form.email (editorData.expert.email /= form.email)
            }
    in
    createEvent (EditExpertEvent data) editorData.expert.uuid editorData.parentUuid


createDeleteExpertEvent : String -> String -> Seed -> ( Event, Seed )
createDeleteExpertEvent =
    createEvent DeleteExpertEvent


createMoveQuestionEvent : String -> String -> String -> Seed -> ( Event, Seed )
createMoveQuestionEvent =
    createMoveEvent MoveQuestionEvent


createMoveAnswerEvent : String -> String -> String -> Seed -> ( Event, Seed )
createMoveAnswerEvent =
    createMoveEvent MoveAnswerEvent


createMoveChoiceEvent : String -> String -> String -> Seed -> ( Event, Seed )
createMoveChoiceEvent =
    createMoveEvent MoveChoiceEvent


createMoveReferenceEvent : String -> String -> String -> Seed -> ( Event, Seed )
createMoveReferenceEvent =
    createMoveEvent MoveReferenceEvent


createMoveExpertEvent : String -> String -> String -> Seed -> ( Event, Seed )
createMoveExpertEvent =
    createMoveEvent MoveExpertEvent


createMoveEvent : (MoveEventData -> CommonEventData -> Event) -> String -> String -> String -> Seed -> ( Event, Seed )
createMoveEvent constructor targetUuid =
    let
        data =
            { targetUuid = targetUuid }
    in
    createEvent (constructor data)


createEvent : (CommonEventData -> Event) -> String -> String -> Seed -> ( Event, Seed )
createEvent create entityUuid parentUuid seed =
    let
        ( uuid, newSeed ) =
            getUuidString seed

        event =
            create
                { uuid = uuid
                , parentUuid = parentUuid
                , entityUuid = entityUuid
                }
    in
    ( event, newSeed )
