module KMEditor.Editor.Update.Events exposing (..)

import KMEditor.Common.Models.Entities exposing (Reference(..), getReferenceUuid)
import KMEditor.Common.Models.Events exposing (..)
import KMEditor.Common.Models.Path exposing (Path)
import KMEditor.Editor.Models.Editors exposing (..)
import KMEditor.Editor.Models.Forms exposing (..)
import Random.Pcg exposing (Seed)
import Utils exposing (getUuid)


createEditKnowledgeModelEvent : KnowledgeModelForm -> KMEditorData -> Seed -> ( Event, Seed )
createEditKnowledgeModelEvent form editorData =
    let
        data =
            { kmUuid = editorData.knowledgeModel.uuid
            , name = createEventField form.name (editorData.knowledgeModel.name /= form.name)
            , chapterIds = createEventField editorData.chapters.list editorData.chapters.dirty
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
            , questionIds = createEventField editorData.questions.list editorData.questions.dirty
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


createAddQuestionEvent : QuestionForm -> QuestionEditorData -> Seed -> ( Event, Seed )
createAddQuestionEvent form editorData =
    let
        maybeAnswerItemTemlate =
            if form.type_ == "list" then
                Just
                    { title = form.itemName
                    , questionIds = editorData.answerItemTemplateQuestions.list
                    }
            else
                Nothing

        data =
            { questionUuid = editorData.question.uuid
            , type_ = form.type_
            , title = form.title
            , shortQuestionUuid = form.shortUuid
            , text = form.text
            , answerItemTemplate = maybeAnswerItemTemlate
            }
    in
    createEvent (AddQuestionEvent data) editorData.path


createEditQuestionEvent : QuestionForm -> QuestionEditorData -> Seed -> ( Event, Seed )
createEditQuestionEvent form editorData =
    let
        maybeAnswerIds =
            if form.type_ == "options" then
                Just editorData.answers.list
            else
                Nothing

        answerIdsChanged =
            editorData.answers.dirty || ((form.type_ == "options" || editorData.question.type_ == "options") && form.type_ /= editorData.question.type_)

        maybeAnswerItemTemlate =
            if form.type_ == "list" then
                Just
                    { title = form.itemName
                    , questionIds = editorData.answerItemTemplateQuestions.list
                    }
            else
                Nothing

        answerItemTemplateChanged =
            editorData.answerItemTemplateQuestions.dirty || ((form.type_ == "list" || editorData.question.type_ == "list") && form.type_ /= editorData.question.type_)

        data =
            { questionUuid = editorData.question.uuid
            , type_ = createEventField form.type_ (editorData.question.type_ /= form.type_)
            , title = createEventField form.title (editorData.question.title /= form.title)
            , shortQuestionUuid = createEventField form.shortUuid (editorData.question.shortUuid /= form.shortUuid)
            , text = createEventField form.text (editorData.question.text /= form.text)
            , answerItemTemplate = createEventField maybeAnswerItemTemlate answerItemTemplateChanged
            , answerIds = createEventField maybeAnswerIds answerIdsChanged
            , referenceIds = createEventField editorData.references.list editorData.references.dirty
            , expertIds = createEventField editorData.experts.list editorData.experts.dirty
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
            }
    in
    createEvent (AddAnswerEvent data) editorData.path


createEditAnswerEvent : AnswerForm -> AnswerEditorData -> Seed -> ( Event, Seed )
createEditAnswerEvent form editorData =
    let
        data =
            { answerUuid = editorData.answer.uuid
            , label = createEventField form.label (editorData.answer.label /= form.label)
            , advice = createEventField form.advice (editorData.answer.advice /= form.advice)
            , followUpIds = createEventField editorData.followUps.list editorData.followUps.dirty
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

                URLReferenceFormType url anchor ->
                    AddURLReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , url = url
                        , anchor = anchor
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
                        ResourcePageReference data ->
                            field data /= newValue

                        _ ->
                            True
            in
            createEventField newValue changed

        urlEventField field newValue =
            let
                changed =
                    case editorData.reference of
                        URLReference data ->
                            field data /= newValue

                        _ ->
                            True
            in
            createEventField newValue changed

        crossEventField field newValue =
            let
                changed =
                    case editorData.reference of
                        CrossReference data ->
                            field data /= newValue

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

                URLReferenceFormType url anchor ->
                    EditURLReferenceEvent
                        { referenceUuid = getReferenceUuid editorData.reference
                        , url = urlEventField .url url
                        , anchor = urlEventField .anchor anchor
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
