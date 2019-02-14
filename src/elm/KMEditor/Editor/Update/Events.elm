module KMEditor.Editor.Update.Events exposing
    ( createAddAnswerEvent
    , createAddChapterEvent
    , createAddExpertEvent
    , createAddQuestionEvent
    , createAddReferenceEvent
    , createAddTagEvent
    , createDeleteAnswerEvent
    , createDeleteChapterEvent
    , createDeleteExpertEvent
    , createDeleteQuestionEvent
    , createDeleteReferenceEvent
    , createDeleteTagEvent
    , createEditAnswerEvent
    , createEditChapterEvent
    , createEditExpertEvent
    , createEditKnowledgeModelEvent
    , createEditQuestionEvent
    , createEditReferenceEvent
    , createEditTagEvent
    )

import KMEditor.Common.Models.Entities exposing (..)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Common.Models.Path exposing (Path)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (..)
import Random exposing (Seed)
import Utils exposing (getUuid)


createEditKnowledgeModelEvent : KnowledgeModelForm -> KMEditorData -> Seed -> ( Event, Seed )
createEditKnowledgeModelEvent form editorData =
    let
        data =
            { kmUuid = editorData.knowledgeModel.uuid
            , name = createEventField form.name (editorData.knowledgeModel.name /= form.name)
            , chapterUuids = createEventField editorData.chapters.list editorData.chapters.dirty
            , tagUuids = createEventField editorData.tags.list editorData.tags.dirty
            }
    in
    createEvent (EditKnowledgeModelEvent data) editorData.path


createAddChapterEvent : ChapterForm -> ChapterEditorData -> Seed -> ( Event, Seed )
createAddChapterEvent form editorData =
    let
        data =
            { chapterUuid = editorData.chapter.uuid
            , title = form.title
            , text = form.text
            }
    in
    createEvent (AddChapterEvent data) editorData.path


createEditChapterEvent : ChapterForm -> ChapterEditorData -> Seed -> ( Event, Seed )
createEditChapterEvent form editorData =
    let
        data =
            { chapterUuid = editorData.chapter.uuid
            , title = createEventField form.title (editorData.chapter.title /= form.title)
            , text = createEventField form.text (editorData.chapter.text /= form.text)
            , questionUuids = createEventField editorData.questions.list editorData.questions.dirty
            }
    in
    createEvent (EditChapterEvent data) editorData.path


createDeleteChapterEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteChapterEvent chapterUuid =
    let
        data =
            { chapterUuid = chapterUuid
            }
    in
    createEvent (DeleteChapterEvent data)


createAddTagEvent : TagForm -> TagEditorData -> Seed -> ( Event, Seed )
createAddTagEvent form editorData =
    let
        data =
            { tagUuid = editorData.tag.uuid
            , name = form.name
            , description = form.description
            , color = form.color
            }
    in
    createEvent (AddTagEvent data) editorData.path


createEditTagEvent : TagForm -> TagEditorData -> Seed -> ( Event, Seed )
createEditTagEvent form editorData =
    let
        data =
            { tagUuid = editorData.tag.uuid
            , name = createEventField form.name (editorData.tag.name /= form.name)
            , description = createEventField form.description (editorData.tag.description /= form.description)
            , color = createEventField form.color (editorData.tag.color /= form.color)
            }
    in
    createEvent (EditTagEvent data) editorData.path


createDeleteTagEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteTagEvent tagUuid =
    let
        data =
            { tagUuid = tagUuid
            }
    in
    createEvent (DeleteTagEvent data)


createAddQuestionEvent : QuestionForm -> QuestionEditorData -> Seed -> ( Event, Seed )
createAddQuestionEvent form editorData =
    let
        data =
            case form.question of
                OptionsQuestionForm formData ->
                    AddOptionsQuestionEvent
                        { questionUuid = getQuestionUuid editorData.question
                        , title = formData.title
                        , text = formData.text
                        , requiredLevel = formData.requiredLevel
                        , tagUuids = editorData.tagUuids
                        }

                ListQuestionForm formData ->
                    AddListQuestionEvent
                        { questionUuid = getQuestionUuid editorData.question
                        , title = formData.title
                        , text = formData.text
                        , requiredLevel = formData.requiredLevel
                        , tagUuids = editorData.tagUuids
                        , itemTitle = formData.itemTitle
                        }

                ValueQuestionForm formData ->
                    AddValueQuestionEvent
                        { questionUuid = getQuestionUuid editorData.question
                        , title = formData.title
                        , text = formData.text
                        , requiredLevel = formData.requiredLevel
                        , tagUuids = editorData.tagUuids
                        , valueType = formData.valueType
                        }
    in
    createEvent (AddQuestionEvent data) editorData.path


createEditQuestionEvent : QuestionForm -> QuestionEditorData -> Seed -> ( Event, Seed )
createEditQuestionEvent form editorData =
    let
        data =
            case form.question of
                OptionsQuestionForm formData ->
                    EditOptionsQuestionEvent
                        { questionUuid = getQuestionUuid editorData.question
                        , title = createEventField formData.title (getQuestionTitle editorData.question /= formData.title)
                        , text = createEventField formData.text (getQuestionText editorData.question /= formData.text)
                        , requiredLevel = createEventField formData.requiredLevel (getQuestionRequiredLevel editorData.question /= formData.requiredLevel)
                        , tagUuids = createEventField editorData.tagUuids (getQuestionTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = createEventField editorData.references.list editorData.references.dirty
                        , expertUuids = createEventField editorData.experts.list editorData.experts.dirty
                        , answerUuids = createEventField editorData.answers.list editorData.answers.dirty
                        }

                ListQuestionForm formData ->
                    EditListQuestionEvent
                        { questionUuid = getQuestionUuid editorData.question
                        , title = createEventField formData.title (getQuestionTitle editorData.question /= formData.title)
                        , text = createEventField formData.text (getQuestionText editorData.question /= formData.text)
                        , requiredLevel = createEventField formData.requiredLevel (getQuestionRequiredLevel editorData.question /= formData.requiredLevel)
                        , tagUuids = createEventField editorData.tagUuids (getQuestionTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = createEventField editorData.references.list editorData.references.dirty
                        , expertUuids = createEventField editorData.experts.list editorData.experts.dirty
                        , itemTitle = createEventField formData.itemTitle (getQuestionItemTitle editorData.question /= formData.itemTitle)
                        , itemQuestionUuids = createEventField editorData.itemQuestions.list editorData.itemQuestions.dirty
                        }

                ValueQuestionForm formData ->
                    EditValueQuestionEvent
                        { questionUuid = getQuestionUuid editorData.question
                        , title = createEventField formData.title (getQuestionTitle editorData.question /= formData.title)
                        , text = createEventField formData.text (getQuestionText editorData.question /= formData.text)
                        , requiredLevel = createEventField formData.requiredLevel (getQuestionRequiredLevel editorData.question /= formData.requiredLevel)
                        , tagUuids = createEventField editorData.tagUuids (getQuestionTagUuids editorData.question /= editorData.tagUuids)
                        , referenceUuids = createEventField editorData.references.list editorData.references.dirty
                        , expertUuids = createEventField editorData.experts.list editorData.experts.dirty
                        , valueType = createEventField formData.valueType (getQuestionValueType editorData.question /= Just formData.valueType)
                        }
    in
    createEvent (EditQuestionEvent data) editorData.path


createDeleteQuestionEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteQuestionEvent questionUuid =
    let
        data =
            { questionUuid = questionUuid
            }
    in
    createEvent (DeleteQuestionEvent data)


createAddAnswerEvent : AnswerForm -> AnswerEditorData -> Seed -> ( Event, Seed )
createAddAnswerEvent form editorData =
    let
        data =
            { answerUuid = editorData.answer.uuid
            , label = form.label
            , advice = form.advice
            , metricMeasures = getMetricMesures form
            }
    in
    createEvent (AddAnswerEvent data) editorData.path


createEditAnswerEvent : AnswerForm -> AnswerEditorData -> Seed -> ( Event, Seed )
createEditAnswerEvent form editorData =
    let
        metricMeasures =
            getMetricMesures form

        data =
            { answerUuid = editorData.answer.uuid
            , label = createEventField form.label (editorData.answer.label /= form.label)
            , advice = createEventField form.advice (editorData.answer.advice /= form.advice)
            , metricMeasures = createEventField metricMeasures (editorData.answer.metricMeasures /= metricMeasures)
            , followUpUuids = createEventField editorData.followUps.list editorData.followUps.dirty
            }
    in
    createEvent (EditAnswerEvent data) editorData.path


createDeleteAnswerEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteAnswerEvent answerUuid =
    let
        data =
            { answerUuid = answerUuid
            }
    in
    createEvent (DeleteAnswerEvent data)


createAddReferenceEvent : ReferenceForm -> ReferenceEditorData -> Seed -> ( Event, Seed )
createAddReferenceEvent form editorData =
    let
        data =
            case form.reference of
                ResourcePageReferenceFormType shortUuid ->
                    AddResourcePageReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , shortUuid = shortUuid
                        }

                URLReferenceFormType url label ->
                    AddURLReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , url = url
                        , label = label
                        }

                CrossReferenceFormType targetUuid description ->
                    AddCrossReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , targetUuid = targetUuid
                        , description = description
                        }
    in
    createEvent (AddReferenceEvent data) editorData.path


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
            createEventField newValue changed

        urlEventField field newValue =
            let
                changed =
                    case editorData.reference of
                        URLReference urlData ->
                            field urlData /= newValue

                        _ ->
                            True
            in
            createEventField newValue changed

        crossEventField field newValue =
            let
                changed =
                    case editorData.reference of
                        CrossReference crossReferenceData ->
                            field crossReferenceData /= newValue

                        _ ->
                            True
            in
            createEventField newValue changed

        data =
            case form.reference of
                ResourcePageReferenceFormType shortUuid ->
                    EditResourcePageReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , shortUuid = resourcePageEventField .shortUuid shortUuid
                        }

                URLReferenceFormType url label ->
                    EditURLReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , url = urlEventField .url url
                        , label = urlEventField .label label
                        }

                CrossReferenceFormType targetUuid description ->
                    EditCrossReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , targetUuid = crossEventField .targetUuid targetUuid
                        , description = crossEventField .description description
                        }
    in
    createEvent (EditReferenceEvent data) editorData.path


createDeleteReferenceEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteReferenceEvent referenceUuid =
    let
        data =
            { referenceUuid = referenceUuid
            }
    in
    createEvent (DeleteReferenceEvent data)


createAddExpertEvent : ExpertForm -> ExpertEditorData -> Seed -> ( Event, Seed )
createAddExpertEvent form editorData =
    let
        data =
            { expertUuid = editorData.expert.uuid
            , name = form.name
            , email = form.email
            }
    in
    createEvent (AddExpertEvent data) editorData.path


createEditExpertEvent : ExpertForm -> ExpertEditorData -> Seed -> ( Event, Seed )
createEditExpertEvent form editorData =
    let
        data =
            { expertUuid = editorData.expert.uuid
            , name = createEventField form.name (editorData.expert.name /= form.name)
            , email = createEventField form.email (editorData.expert.email /= form.email)
            }
    in
    createEvent (EditExpertEvent data) editorData.path


createDeleteExpertEvent : String -> Path -> Seed -> ( Event, Seed )
createDeleteExpertEvent expertUuid =
    let
        data =
            { expertUuid = expertUuid
            }
    in
    createEvent (DeleteExpertEvent data)


createEvent : (CommonEventData -> Event) -> Path -> Seed -> ( Event, Seed )
createEvent create path seed =
    let
        ( uuid, newSeed ) =
            getUuid seed

        event =
            create
                { uuid = uuid
                , path = path
                }
    in
    ( event, newSeed )


createEventField : a -> Bool -> EventField a
createEventField value changed =
    let
        v =
            if changed then
                Just value

            else
                Nothing
    in
    { changed = changed
    , value = v
    }
