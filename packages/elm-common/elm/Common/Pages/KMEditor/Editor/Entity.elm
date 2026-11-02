module Common.Pages.KMEditor.Editor.Entity exposing (Config, fieldId, paneId, view)

{-| Whatever the tree has selected, in the middle of the workspace: what kind of
thing it is, what can be done to the whole of it, and its fields.

Every field sends its change as an **event** rather than setting state: the
widget's `onInput` builds an `Edit…EventData` with one field marked changed, and
the shell applies it locally and puts it on the wire. A partial patch is what
the protocol carries, so a field nobody touched is not sent.

Typing is debounced; a change of kind, a reorder and an add are not, because a
reorder squashed onto the edit before it would be a different knowledge model.

The integration form — the API request, its test and the response mapping — is
the one editor not here yet; it is the largest single form in the wizard.

-}

import Common.Api.Models.Event.AddAnswerEventData as AddAnswerEventData
import Common.Api.Models.Event.AddChapterEventData as AddChapterEventData
import Common.Api.Models.Event.AddChoiceEventData as AddChoiceEventData
import Common.Api.Models.Event.AddExpertEventData as AddExpertEventData
import Common.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Common.Api.Models.Event.AddMetricEventData as AddMetricEventData
import Common.Api.Models.Event.AddPhaseEventData as AddPhaseEventData
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Common.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Common.Api.Models.Event.AddResourceCollectionEventData as AddResourceCollectionEventData
import Common.Api.Models.Event.AddResourcePageEventData as AddResourcePageEventData
import Common.Api.Models.Event.AddTagEventData as AddTagEventData
import Common.Api.Models.Event.EditAnswerEventData as EditAnswerEventData
import Common.Api.Models.Event.EditChapterEventData as EditChapterEventData
import Common.Api.Models.Event.EditChoiceEventData as EditChoiceEventData
import Common.Api.Models.Event.EditEventSetters exposing (setAbbreviation, setAdvice, setAnnotations, setAnswerUuids, setChapterUuids, setChoiceUuids, setColor, setContent, setDescription, setEmail, setFileTypes, setFollowUpUuids, setIntegrationUuid, setIntegrationUuids, setItemTemplateQuestionUuids, setLabel, setListQuestionUuid, setMaxSize, setMetricMeasures, setMetricUuids, setName, setPhaseUuids, setQuestionUuids, setResourceCollectionUuids, setResourcePageUuid, setResourcePageUuids, setTagUuids, setTargetUuid, setText, setTitle, setUrl, setValidations, setValueType, setVariables)
import Common.Api.Models.Event.EditExpertEventData as EditExpertEventData
import Common.Api.Models.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData
import Common.Api.Models.Event.EditMetricEventData as EditMetricEventData
import Common.Api.Models.Event.EditPhaseEventData as EditPhaseEventData
import Common.Api.Models.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Common.Api.Models.Event.EditQuestionFileEventData as EditQuestionFileEventData
import Common.Api.Models.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData
import Common.Api.Models.Event.EditQuestionItemSelectData as EditQuestionItemSelectEventData
import Common.Api.Models.Event.EditQuestionListEventData as EditQuestionListEventData
import Common.Api.Models.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData
import Common.Api.Models.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData
import Common.Api.Models.Event.EditQuestionValueEventData as EditQuestionValueEventData
import Common.Api.Models.Event.EditReferenceCrossEventData as EditReferenceCrossEventData
import Common.Api.Models.Event.EditReferenceEventData exposing (EditReferenceEventData(..))
import Common.Api.Models.Event.EditReferenceResourcePageEventData as EditReferenceResourcePageEventData
import Common.Api.Models.Event.EditReferenceURLEventData as EditReferenceURLEventData
import Common.Api.Models.Event.EditResourceCollectionEventData as EditResourceCollectionEventData
import Common.Api.Models.Event.EditResourcePageEventData as EditResourcePageEventData
import Common.Api.Models.Event.EditTagEventData as EditTagEventData
import Common.Api.Models.Event.EventContent exposing (EventContent(..))
import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Common.Api.Models.KnowledgeModel.Answer exposing (Answer)
import Common.Api.Models.KnowledgeModel.Chapter exposing (Chapter)
import Common.Api.Models.KnowledgeModel.Choice exposing (Choice)
import Common.Api.Models.KnowledgeModel.Expert exposing (Expert)
import Common.Api.Models.KnowledgeModel.Integration as Integration exposing (Integration)
import Common.Api.Models.KnowledgeModel.Metric exposing (Metric)
import Common.Api.Models.KnowledgeModel.MetricMeasure as MetricMeasure
import Common.Api.Models.KnowledgeModel.Phase exposing (Phase)
import Common.Api.Models.KnowledgeModel.Question as Question exposing (Question(..))
import Common.Api.Models.KnowledgeModel.Question.QuestionValueType as QuestionValueType exposing (QuestionValueType(..))
import Common.Api.Models.KnowledgeModel.Reference as Reference exposing (Reference(..))
import Common.Api.Models.KnowledgeModel.ResourceCollection exposing (ResourceCollection)
import Common.Api.Models.KnowledgeModel.ResourcePage exposing (ResourcePage)
import Common.Api.Models.KnowledgeModel.Tag exposing (Tag)
import Common.Components.Copyable as Copyable
import Common.Pages.KMEditor.Editor.EditorContext as EditorContext exposing (EditorContext)
import Common.Pages.KMEditor.Editor.Integration as IntegrationForm
import Common.Pages.KMEditor.Editor.Move as Move
import Common.Pages.KMEditor.Editor.QuestionEvent as QuestionEvent
import Common.Pages.KMEditor.Editor.Validations as Validations
import Common.Pages.KMEditor.Editor.Widgets as Widgets
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import List.Extra as List
import Maybe.Extra as Maybe
import Ui.Alert as Alert
import Ui.Button as Button
import Ui.Copy as Copy
import Ui.Field as Field
import Ui.Form as UiForm
import Ui.Icon as Icon
import Ui.Illustration as Illustration
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Page.Message as Message
import Ui.ReorderableList as ReorderableList
import Ui.Text as Text
import Uuid


type alias Config msg =
    { locale : Gettext.Locale

    -- debounce? · where to put focus back · parent uuid · entity uuid · what happened
    , eventMsg : Bool -> Maybe String -> String -> Maybe String -> EventContent -> msg

    -- Where an entity's own page is, for the links in its parent's list.
    , entityUrl : String -> String

    -- The tenant's secrets, named so an integration's request can use them.
    , secrets : List String

    -- The largest file this tenant accepts, which a file question cannot ask
    -- for more than.
    , maxUploadFileSize : Int

    -- The uuid in the header goes on the clipboard, for a document template to
    -- name this entity by.
    , copy : Copy.Model
    , copyMsg : Copy.Msg -> msg

    -- Sending this entity somewhere else in the knowledge model.
    , onMove : Move.MovingEntity -> String -> msg

    -- Deleting it, which is asked about first: the shell holds the question.
    , onDelete : { kind : String, uuid : String, event : EventContent } -> msg

    -- The integration editor needs more of the shell than any other form —
    -- a request in flight, what came back, where a caret was — so the shell
    -- hands it its whole configuration rather than a dozen more fields here.
    , integration : String -> IntegrationForm.Config msg

    -- A number being typed. The stored value cannot render back what is being
    -- typed — "0." parses to 0, which would rub the point out from under the
    -- caret, and "0.50" would lose its nought — so the text stands as typed
    -- until the field is left, and the stored value takes over again then.
    , draft : String -> Maybe String
    , onDraft : String -> String -> msg -> msg
    , onDraftDone : String -> msg

    -- A child being dragged onto another place in its list. The shell holds it
    -- because it is a thing being done rather than a thing being shown.
    , childDrag : Maybe ReorderableList.Dragging
    , onChildDrag : Maybe ReorderableList.Dragging -> msg
    }


view : Config msg -> EditorContext -> Html msg
view config editorContext =
    let
        uuid =
            editorContext.activeUuid

        km =
            editorContext.kmEditor.knowledgeModel
    in
    div [ class "c-km-editor-entity", id paneId ]
        [ if uuid == Uuid.toString km.uuid then
            frame config
                editorContext
                uuid
                { kind = gettext "Knowledge Model" config.locale
                , deleteEvent = Nothing
                , fields = knowledgeModelForm config editorContext
                }

          else
            case find config editorContext uuid of
                Just found ->
                    frame config editorContext uuid found

                Nothing ->
                    Message.message
                        { title = gettext "Nothing selected" config.locale
                        , description = gettext "Choose a chapter, a question or anything else in the knowledge model to edit it." config.locale
                        }
                        |> Message.withIllustration (Illustration.toHtml Illustration.placeholder)
                        |> Message.withinPage
                        |> Message.toHtml
        ]


{-| The pane the forms are drawn in. The shell focuses the first field inside it
when the tree opens something, so the id has to be somewhere both can name it.
-}
paneId : String
paneId =
    "km-editor-entity"


type alias Form msg =
    { kind : String
    , deleteEvent : Maybe EventContent
    , fields : List (Html msg)
    }


{-| The row every form starts with: what kind of thing this is, and what can be
done to the whole of it.

**The name is not repeated here.** It is the first field of every form, an inch
below, and a title that echoes an input is a title that goes stale between
keystrokes.

Copying the uuid, moving and deleting all act on this entity rather than on any
one field of it, which is why they are in the header together rather than a
button at the foot of the form. The knowledge model itself offers no uuid to copy.

-}
frame : Config msg -> EditorContext -> String -> Form msg -> Html msg
frame config editorContext uuid form =
    let
        deleted =
            EditorContext.isDeleted uuid editorContext
    in
    Layout.stack
        (div [ class "c-km-editor-entity__header" ]
            [ Text.title form.kind
            , div [ class "c-km-editor-entity__actions" ] (entityActions config editorContext uuid form deleted)
            ]
            :: (if deleted then
                    [ Alert.toHtml (Alert.warning (gettext "This has been deleted. It goes when the knowledge model is published." config.locale)) ]

                else
                    form.fields
               )
        )


entityActions : Config msg -> EditorContext -> String -> Form msg -> Bool -> List (Html msg)
entityActions config editorContext uuid form deleted =
    let
        copyUuid =
            if uuid == Uuid.toString editorContext.kmEditor.knowledgeModel.uuid then
                []

            else
                [ Copyable.brief { locale = config.locale }
                    { key = "km-editor-uuid"
                    , label = gettext "Copy UUID" config.locale
                    , chars = 8
                    , value = uuid
                    , model = config.copy
                    , toMsg = config.copyMsg
                    }
                ]

        moveAction =
            case ( deleted, Move.movingEntity uuid editorContext ) of
                ( False, Just entity ) ->
                    [ Button.toHtml
                        (Button.secondary (gettext "Move" config.locale) (config.onMove entity uuid)
                            |> Button.withIcon (Icon.solid "fa-shuffle")
                            |> Button.small
                            |> Button.withTestId "km-editor_move"
                        )
                    ]

                _ ->
                    []

        deleteAction =
            case ( deleted, form.deleteEvent ) of
                ( False, Just deleteEvent ) ->
                    [ Button.toHtml
                        (Button.dangerSecondary (gettext "Delete" config.locale)
                            (config.onDelete { kind = form.kind, uuid = uuid, event = deleteEvent })
                            |> Button.withIcon (Icon.solid "fa-trash")
                            |> Button.small
                            |> Button.withTestId "km-editor_delete"
                        )
                    ]

                _ ->
                    []
    in
    copyUuid ++ moveAction ++ deleteAction


find : Config msg -> EditorContext -> String -> Maybe (Form msg)
find config editorContext uuid =
    let
        km =
            editorContext.kmEditor.knowledgeModel

        entity get build =
            Maybe.map (build config editorContext) (get uuid km)
    in
    [ entity KnowledgeModel.getChapter chapterForm
    , entity KnowledgeModel.getQuestion questionForm
    , entity KnowledgeModel.getAnswer answerForm
    , entity KnowledgeModel.getChoice choiceForm
    , entity KnowledgeModel.getReference referenceForm
    , entity KnowledgeModel.getExpert expertForm
    , entity KnowledgeModel.getMetric metricForm
    , entity KnowledgeModel.getPhase phaseForm
    , entity KnowledgeModel.getTag tagForm
    , entity KnowledgeModel.getIntegration integrationForm
    , entity KnowledgeModel.getResourceCollection resourceCollectionForm
    , entity KnowledgeModel.getResourcePage resourcePageForm
    ]
        |> Maybe.values
        |> List.head



-- SENDING AN EDIT


{-| An edit of one field of one entity: the setter marks that field changed, and
everything else goes on the wire as untouched.
-}
edit : Config msg -> EditorContext -> String -> (data -> EventContent) -> data -> msg
edit config editorContext uuid wrap data =
    config.eventMsg True Nothing (EditorContext.getParentUuid uuid editorContext) (Just uuid) (wrap data)


{-| An edit that has to put focus back where it was. The annotations editor
rebuilds its rows, so the caret would otherwise land at the top of the page.
-}
editWithFocus : Config msg -> EditorContext -> String -> String -> (data -> EventContent) -> data -> msg
editWithFocus config editorContext uuid selector wrap data =
    config.eventMsg True (Just selector) (EditorContext.getParentUuid uuid editorContext) (Just uuid) (wrap data)


{-| A change that is not typing: a reorder, a change of kind. It goes at once.
-}
structural : Config msg -> EditorContext -> String -> (data -> EventContent) -> data -> msg
structural config editorContext uuid wrap data =
    config.eventMsg False Nothing (EditorContext.getParentUuid uuid editorContext) (Just uuid) (wrap data)


{-| Making a child of this entity: the parent is this one, and the child's uuid
is the shell's to make.
-}
add : Config msg -> String -> EventContent -> msg
add config parentUuid content =
    config.eventMsg False Nothing parentUuid Nothing content


annotationsField : Config msg -> EditorContext -> String -> List Annotation -> (List Annotation -> EventContent) -> Html msg
annotationsField config editorContext uuid value wrap =
    Widgets.annotations config
        { id = fieldId uuid "annotations"
        , value = value
        , onChange = editWithFocus config editorContext uuid ("#" ++ fieldId uuid "annotations") wrap
        }


fieldId : String -> String -> String
fieldId uuid name =
    "km-editor-" ++ uuid ++ "-" ++ name



-- THE FORMS


knowledgeModelForm : Config msg -> EditorContext -> List (Html msg)
knowledgeModelForm config editorContext =
    let
        km =
            editorContext.kmEditor.knowledgeModel

        uuid =
            Uuid.toString km.uuid

        list label name uuids nameOf setter addContent addLabel =
            childrenField config
                editorContext
                uuid
                { name = name
                , label = label
                , uuids = uuids
                , nameOf = nameOf
                , onReorder = \order -> structural config editorContext uuid EditKnowledgeModelEvent (setter order EditKnowledgeModelEventData.init)
                , onAdd = add config uuid addContent
                , addLabel = addLabel
                }
    in
    [ list (gettext "Chapters" config.locale) "chapters" km.chapterUuids (KnowledgeModel.getChapterName km) setChapterUuids (AddChapterEvent AddChapterEventData.init) (gettext "Add chapter" config.locale)
    , list (gettext "Metrics" config.locale) "metrics" km.metricUuids (KnowledgeModel.getMetricName km) setMetricUuids (AddMetricEvent AddMetricEventData.init) (gettext "Add metric" config.locale)
    , list (gettext "Phases" config.locale) "phases" km.phaseUuids (KnowledgeModel.getPhaseName km) setPhaseUuids (AddPhaseEvent AddPhaseEventData.init) (gettext "Add phase" config.locale)
    , list (gettext "Question Tags" config.locale) "tags" km.tagUuids (KnowledgeModel.getTagName km) setTagUuids (AddTagEvent AddTagEventData.init) (gettext "Add question tag" config.locale)
    , list (gettext "Integrations" config.locale) "integrations" km.integrationUuids (KnowledgeModel.getIntegrationName km) setIntegrationUuids (AddIntegrationEvent AddIntegrationEventData.init) (gettext "Add integration" config.locale)
    , list (gettext "Resource Collections" config.locale) "resourceCollections" km.resourceCollectionUuids (KnowledgeModel.getResourceCollectionName km) setResourceCollectionUuids (AddResourceCollectionEvent AddResourceCollectionEventData.init) (gettext "Add resource collection" config.locale)
    , annotationsField config editorContext uuid km.annotations (\value -> EditKnowledgeModelEvent (setAnnotations value EditKnowledgeModelEventData.init))
    ]


chapterForm : Config msg -> EditorContext -> Chapter -> Form msg
chapterForm config editorContext chapter =
    let
        uuid =
            chapter.uuid

        change setter value =
            edit config editorContext uuid EditChapterEvent (setter value EditChapterEventData.init)
    in
    { kind = gettext "Chapter" config.locale
    , deleteEvent = Just DeleteChapterEvent
    , fields =
        [ Widgets.string
            { id = fieldId uuid "title", label = gettext "Title" config.locale, value = chapter.title, onInput = change setTitle }
        , Widgets.markdown
            { id = fieldId uuid "text", label = gettext "Text" config.locale, value = Maybe.withDefault "" chapter.text, onInput = change setText << toMaybe }
        , childrenField config
            editorContext
            uuid
            { name = "questions"
            , label = gettext "Questions" config.locale
            , uuids = chapter.questionUuids
            , nameOf = KnowledgeModel.getQuestionName editorContext.kmEditor.knowledgeModel
            , onReorder = \order -> structural config editorContext uuid EditChapterEvent (setQuestionUuids order EditChapterEventData.init)
            , onAdd = add config uuid (AddQuestionEvent AddQuestionEventData.init)
            , addLabel = gettext "Add question" config.locale
            }
        , annotationsField config editorContext uuid chapter.annotations (\value -> EditChapterEvent (setAnnotations value EditChapterEventData.init))
        ]
    }


answerForm : Config msg -> EditorContext -> Answer -> Form msg
answerForm config editorContext answer =
    let
        uuid =
            answer.uuid

        change setter value =
            edit config editorContext uuid EditAnswerEvent (setter value EditAnswerEventData.init)
    in
    { kind = gettext "Answer" config.locale
    , deleteEvent = Just DeleteAnswerEvent
    , fields =
        [ Widgets.string
            { id = fieldId uuid "label", label = gettext "Label" config.locale, value = answer.label, onInput = change setLabel }
        , Widgets.markdown
            { id = fieldId uuid "advice", label = gettext "Advice" config.locale, value = Maybe.withDefault "" answer.advice, onInput = change setAdvice << toMaybe }
        , childrenField config
            editorContext
            uuid
            { name = "followUps"
            , label = gettext "Follow-up Questions" config.locale
            , uuids = answer.followUpUuids
            , nameOf = KnowledgeModel.getQuestionName editorContext.kmEditor.knowledgeModel
            , onReorder = \order -> structural config editorContext uuid EditAnswerEvent (setFollowUpUuids order EditAnswerEventData.init)
            , onAdd = add config uuid (AddQuestionEvent AddQuestionEventData.init)
            , addLabel = gettext "Add follow-up question" config.locale
            }
        ]
            ++ metricsField config editorContext answer
            ++ [ annotationsField config editorContext uuid answer.annotations (\value -> EditAnswerEvent (setAnnotations value EditAnswerEventData.init)) ]
    }


{-| What choosing this answer says about each of the knowledge model's metrics.

Every metric in the knowledge model is listed, and a measure exists for the ones
switched on: the list is the model's, not the answer's, so there is nothing to
add or remove here — only to turn on and fill in. Nothing at all where the
knowledge model has no metrics.

Weight and measure are fractions of one, which is what the server stores and
what the questionnaire's report divides by; a figure outside that would be
silently clamped on the way in, so it is clamped here where it can be seen.

-}
metricsField : Config msg -> EditorContext -> Answer -> List (Html msg)
metricsField config editorContext answer =
    let
        metrics =
            KnowledgeModel.getMetrics editorContext.kmEditor.knowledgeModel
                |> List.filterNot (\metric -> EditorContext.isDeleted metric.uuid editorContext)

        change measures =
            edit config editorContext answer.uuid EditAnswerEvent (setMetricMeasures measures EditAnswerEventData.init)

        measureOf metric =
            List.find (\measure -> measure.metricUuid == metric.uuid) answer.metricMeasures

        onToggle metric isOn =
            change
                (if isOn then
                    answer.metricMeasures ++ [ MetricMeasure.init metric.uuid ]

                 else
                    List.filter (\measure -> measure.metricUuid /= metric.uuid) answer.metricMeasures
                )

        onValue metric setter next =
            change
                (List.map
                    (\measure ->
                        if measure.metricUuid == metric.uuid then
                            setter (clamp 0 1 (Maybe.withDefault 0 (String.toFloat next))) measure

                        else
                            measure
                    )
                    answer.metricMeasures
                )

        amount metric measure name label hint toValue setter =
            let
                key =
                    fieldId answer.uuid ("metric-" ++ metric.uuid ++ "-" ++ name)
            in
            Field.toHtml
                (Field.input
                    { id = key
                    , label = label
                    , value = Maybe.withDefault (String.fromFloat (toValue measure)) (config.draft key)
                    , onInput = \next -> config.onDraft key next (onValue metric setter next)
                    }
                    |> Field.withInputMode "decimal"
                    |> Field.withOnBlur (config.onDraftDone key)
                    |> Field.withHint hint
                )

        metricField metric =
            let
                switch =
                    Widgets.toggle
                        { id = fieldId answer.uuid ("metric-" ++ metric.uuid)
                        , label = metric.title
                        , checked = Maybe.isJust (measureOf metric)
                        , onCheck = onToggle metric
                        }
            in
            case measureOf metric of
                Just measure ->
                    [ switch
                    , UiForm.nested
                        [ amount metric
                            measure
                            "weight"
                            (gettext "Weight" config.locale)
                            (gettext "How much this answer counts towards the metric, from 0 to 1." config.locale)
                            .weight
                            MetricMeasure.setWeight
                        , amount metric
                            measure
                            "measure"
                            (gettext "Measure" config.locale)
                            (gettext "How well this answer scores against the metric: 0 is poor, 1 is good." config.locale)
                            .measure
                            MetricMeasure.setMeasure
                        ]
                    ]

                Nothing ->
                    [ switch ]
    in
    if List.isEmpty metrics then
        []

    else
        [ UiForm.section
            { id = fieldId answer.uuid "metrics"
            , legend = gettext "Metrics" config.locale
            , content = List.concatMap metricField metrics
            }
        ]


choiceForm : Config msg -> EditorContext -> Choice -> Form msg
choiceForm config editorContext choice =
    { kind = gettext "Choice" config.locale
    , deleteEvent = Just DeleteChoiceEvent
    , fields =
        [ Widgets.string
            { id = fieldId choice.uuid "label"
            , label = gettext "Label" config.locale
            , value = choice.label
            , onInput = \value -> edit config editorContext choice.uuid EditChoiceEvent (setLabel value EditChoiceEventData.init)
            }
        , annotationsField config editorContext choice.uuid choice.annotations (\value -> EditChoiceEvent (setAnnotations value EditChoiceEventData.init))
        ]
    }


expertForm : Config msg -> EditorContext -> Expert -> Form msg
expertForm config editorContext expert =
    let
        change setter value =
            edit config editorContext expert.uuid EditExpertEvent (setter value EditExpertEventData.init)
    in
    { kind = gettext "Expert" config.locale
    , deleteEvent = Just DeleteExpertEvent
    , fields =
        [ Widgets.string { id = fieldId expert.uuid "name", label = gettext "Name" config.locale, value = expert.name, onInput = change setName }
        , Widgets.string { id = fieldId expert.uuid "email", label = gettext "Email" config.locale, value = expert.email, onInput = change setEmail }
        , annotationsField config editorContext expert.uuid expert.annotations (\value -> EditExpertEvent (setAnnotations value EditExpertEventData.init))
        ]
    }


metricForm : Config msg -> EditorContext -> Metric -> Form msg
metricForm config editorContext metric =
    let
        change setter value =
            edit config editorContext metric.uuid EditMetricEvent (setter value EditMetricEventData.init)
    in
    { kind = gettext "Metric" config.locale
    , deleteEvent = Just DeleteMetricEvent
    , fields =
        [ Widgets.string { id = fieldId metric.uuid "title", label = gettext "Title" config.locale, value = metric.title, onInput = change setTitle }
        , Widgets.string { id = fieldId metric.uuid "abbreviation", label = gettext "Abbreviation" config.locale, value = Maybe.withDefault "" metric.abbreviation, onInput = change setAbbreviation << toMaybe }
        , Widgets.markdown { id = fieldId metric.uuid "description", label = gettext "Description" config.locale, value = Maybe.withDefault "" metric.description, onInput = change setDescription << toMaybe }
        , annotationsField config editorContext metric.uuid metric.annotations (\value -> EditMetricEvent (setAnnotations value EditMetricEventData.init))
        ]
    }


phaseForm : Config msg -> EditorContext -> Phase -> Form msg
phaseForm config editorContext phase =
    let
        change setter value =
            edit config editorContext phase.uuid EditPhaseEvent (setter value EditPhaseEventData.init)
    in
    { kind = gettext "Phase" config.locale
    , deleteEvent = Just DeletePhaseEvent
    , fields =
        [ Widgets.string { id = fieldId phase.uuid "title", label = gettext "Title" config.locale, value = phase.title, onInput = change setTitle }
        , Widgets.textarea { id = fieldId phase.uuid "description", label = gettext "Description" config.locale, value = Maybe.withDefault "" phase.description, onInput = change setDescription << toMaybe }
        , annotationsField config editorContext phase.uuid phase.annotations (\value -> EditPhaseEvent (setAnnotations value EditPhaseEventData.init))
        ]
    }


tagForm : Config msg -> EditorContext -> Tag -> Form msg
tagForm config editorContext tag =
    let
        change setter value =
            edit config editorContext tag.uuid EditTagEvent (setter value EditTagEventData.init)
    in
    { kind = gettext "Question Tag" config.locale
    , deleteEvent = Just DeleteTagEvent
    , fields =
        [ Widgets.string { id = fieldId tag.uuid "name", label = gettext "Name" config.locale, value = tag.name, onInput = change setName }
        , Widgets.textarea { id = fieldId tag.uuid "description", label = gettext "Description" config.locale, value = Maybe.withDefault "" tag.description, onInput = change setDescription << toMaybe }
        , Widgets.color { id = fieldId tag.uuid "color", label = gettext "Color" config.locale, value = tag.color, onInput = change setColor }
        , annotationsField config editorContext tag.uuid tag.annotations (\value -> EditTagEvent (setAnnotations value EditTagEventData.init))
        ]
    }


resourceCollectionForm : Config msg -> EditorContext -> ResourceCollection -> Form msg
resourceCollectionForm config editorContext collection =
    let
        uuid =
            collection.uuid
    in
    { kind = gettext "Resource Collection" config.locale
    , deleteEvent = Just DeleteResourceCollectionEvent
    , fields =
        [ Widgets.string
            { id = fieldId uuid "title"
            , label = gettext "Title" config.locale
            , value = collection.title
            , onInput = \value -> edit config editorContext uuid EditResourceCollectionEvent (setTitle value EditResourceCollectionEventData.init)
            }
        , childrenField config
            editorContext
            uuid
            { name = "resourcePages"
            , label = gettext "Resource Pages" config.locale
            , uuids = collection.resourcePageUuids
            , nameOf = KnowledgeModel.getResourcePageName editorContext.kmEditor.knowledgeModel
            , onReorder = \order -> structural config editorContext uuid EditResourceCollectionEvent (setResourcePageUuids order EditResourceCollectionEventData.init)
            , onAdd = add config uuid (AddResourcePageEvent AddResourcePageEventData.init)
            , addLabel = gettext "Add resource page" config.locale
            }
        , annotationsField config editorContext uuid collection.annotations (\value -> EditResourceCollectionEvent (setAnnotations value EditResourceCollectionEventData.init))
        ]
    }


resourcePageForm : Config msg -> EditorContext -> ResourcePage -> Form msg
resourcePageForm config editorContext page =
    let
        change setter value =
            edit config editorContext page.uuid EditResourcePageEvent (setter value EditResourcePageEventData.init)
    in
    { kind = gettext "Resource Page" config.locale
    , deleteEvent = Just DeleteResourcePageEvent
    , fields =
        [ Widgets.string { id = fieldId page.uuid "title", label = gettext "Title" config.locale, value = page.title, onInput = change setTitle }
        , Widgets.markdown { id = fieldId page.uuid "content", label = gettext "Content" config.locale, value = page.content, onInput = change setContent }
        , annotationsField config editorContext page.uuid page.annotations (\value -> EditResourcePageEvent (setAnnotations value EditResourcePageEventData.init))
        ]
    }


integrationForm : Config msg -> EditorContext -> Integration -> Form msg
integrationForm config _ integration =
    let
        uuid =
            Integration.getUuid integration
    in
    { kind = gettext "Integration" config.locale
    , deleteEvent = Just DeleteIntegrationEvent
    , fields = IntegrationForm.fields (config.integration uuid) integration
    }



-- THE QUESTION


questionForm : Config msg -> EditorContext -> Question -> Form msg
questionForm config editorContext question =
    let
        uuid =
            Question.getUuid question

        km =
            editorContext.kmEditor.knowledgeModel

        change setter value =
            edit config editorContext uuid EditQuestionEvent (QuestionEvent.questionData question setter value)

        atOnce setter value =
            structural config editorContext uuid EditQuestionEvent (QuestionEvent.questionData question setter value)

        tagOptions =
            List.map (\tag -> { value = tag.uuid, label = tag.name, color = Just tag.color }) (KnowledgeModel.getTags km)

        tagsField =
            if List.isEmpty tagOptions then
                []

            else
                [ Widgets.checkboxes
                    { id = fieldId uuid "tags"
                    , label = gettext "Question Tags" config.locale
                    , values = Question.getTagUuids question
                    , options = tagOptions
                    , onToggle = \tagUuid -> atOnce QuestionEvent.tagSetters (toggle tagUuid (Question.getTagUuids question))
                    }
                ]
    in
    { kind = gettext "Question" config.locale
    , deleteEvent = Just DeleteQuestionEvent
    , fields =
        [ Widgets.selectWithWarning
            (questionTypeWarning config editorContext question)
            { id = fieldId uuid "type"
            , label = gettext "Type" config.locale
            , value = Question.getTypeString question
            , options = questionTypeOptions config
            , onInput = \value -> structural config editorContext uuid EditQuestionEvent (questionTypeChange value)
            }
        , Widgets.string
            { id = fieldId uuid "title", label = gettext "Title" config.locale, value = Question.getTitle question, onInput = change QuestionEvent.titleSetters }
        , Widgets.markdown
            { id = fieldId uuid "text", label = gettext "Text" config.locale, value = Maybe.withDefault "" (Question.getText question), onInput = change QuestionEvent.textSetters << toMaybe }
        , Widgets.select
            { id = fieldId uuid "requiredPhase"
            , label = gettext "When is this required?" config.locale
            , value = Maybe.withDefault "" (Question.getRequiredPhaseUuid question)
            , options = ( "", gettext "Never" config.locale ) :: List.map (\phase -> ( phase.uuid, phase.title )) (KnowledgeModel.getPhases km)
            , onInput = change QuestionEvent.requiredPhaseSetters << toMaybe
            }
        ]
            ++ tagsField
            ++ questionTypeFields config editorContext question
            ++ [ childrenField config
                    editorContext
                    uuid
                    { name = "references"
                    , label = gettext "References" config.locale
                    , uuids = Question.getReferenceUuids question
                    , nameOf = KnowledgeModel.getReferenceName km
                    , onReorder = atOnce QuestionEvent.referenceSetters
                    , onAdd = add config uuid (AddReferenceEvent AddReferenceEventData.init)
                    , addLabel = gettext "Add reference" config.locale
                    }
               , childrenField config
                    editorContext
                    uuid
                    { name = "experts"
                    , label = gettext "Experts" config.locale
                    , uuids = Question.getExpertUuids question
                    , nameOf = KnowledgeModel.getExpertName km
                    , onReorder = atOnce QuestionEvent.expertSetters
                    , onAdd = add config uuid (AddExpertEvent AddExpertEventData.init)
                    , addLabel = gettext "Add expert" config.locale
                    }
               , annotationsField config editorContext uuid (Question.getAnnotations question) (\value -> EditQuestionEvent (QuestionEvent.questionData question QuestionEvent.annotationSetters value))
               ]
    }


{-| What changing the type would throw away. A question's children belong to its
kind: the answers of an options question mean nothing to a value question, and
the server drops them. Only said while there is something to drop, and only for
the three kinds that have children of their own.
-}
questionTypeWarning : Config msg -> EditorContext -> Question -> Maybe String
questionTypeWarning config editorContext question =
    let
        ifAny uuids message =
            if List.isEmpty (EditorContext.filterDeleted editorContext uuids) then
                Nothing

            else
                Just message
    in
    case question of
        OptionsQuestion _ data ->
            ifAny data.answerUuids (gettext "Changing a question type will remove all answers." config.locale)

        ListQuestion _ data ->
            ifAny data.itemTemplateQuestionUuids (gettext "Changing a question type will remove all item questions." config.locale)

        MultiChoiceQuestion _ data ->
            ifAny data.choiceUuids (gettext "Changing a question type will remove all choices." config.locale)

        _ ->
            Nothing


{-| The fields only one kind of question has, and the children that go with it.
-}
questionTypeFields : Config msg -> EditorContext -> Question -> List (Html msg)
questionTypeFields config editorContext question =
    let
        uuid =
            Question.getUuid question

        km =
            editorContext.kmEditor.knowledgeModel

        atOnce data =
            structural config editorContext uuid EditQuestionEvent data

        change data =
            edit config editorContext uuid EditQuestionEvent data
    in
    case question of
        OptionsQuestion _ data ->
            [ childrenField config
                editorContext
                uuid
                { name = "answers"
                , label = gettext "Answers" config.locale
                , uuids = data.answerUuids
                , nameOf = KnowledgeModel.getAnswerName km
                , onReorder = \order -> atOnce (EditQuestionOptionsEvent (setAnswerUuids order EditQuestionOptionsEventData.init))
                , onAdd = add config uuid (AddAnswerEvent AddAnswerEventData.init)
                , addLabel = gettext "Add answer" config.locale
                }
            ]

        ListQuestion _ data ->
            [ childrenField config
                editorContext
                uuid
                { name = "itemQuestions"
                , label = gettext "Item Questions" config.locale
                , uuids = data.itemTemplateQuestionUuids
                , nameOf = KnowledgeModel.getQuestionName km
                , onReorder = \order -> atOnce (EditQuestionListEvent (setItemTemplateQuestionUuids order EditQuestionListEventData.init))
                , onAdd = add config uuid (AddQuestionEvent AddQuestionEventData.init)
                , addLabel = gettext "Add item question" config.locale
                }
            ]

        MultiChoiceQuestion _ data ->
            [ childrenField config
                editorContext
                uuid
                { name = "choices"
                , label = gettext "Choices" config.locale
                , uuids = data.choiceUuids
                , nameOf = KnowledgeModel.getChoiceName km
                , onReorder = \order -> atOnce (EditQuestionMultiChoiceEvent (setChoiceUuids order EditQuestionMultiChoiceEventData.init))
                , onAdd = add config uuid (AddChoiceEvent AddChoiceEventData.init)
                , addLabel = gettext "Add choice" config.locale
                }
            ]

        ValueQuestion _ data ->
            Widgets.select
                { id = fieldId uuid "valueType"
                , label = gettext "Value Type" config.locale
                , value = QuestionValueType.toString data.valueType
                , options = List.map (\valueType -> ( QuestionValueType.toString valueType, QuestionValueType.toReadableString config.locale valueType )) allValueTypes
                , onInput = \value -> change (EditQuestionValueEvent (setValueType (Maybe.withDefault StringQuestionValueType (QuestionValueType.fromString value)) EditQuestionValueEventData.init))
                }
                :: (case
                        Validations.view
                            { locale = config.locale
                            , id = fieldId uuid "validations"
                            , valueType = data.valueType
                            , validations = data.validations
                            , onChange = \validations -> atOnce (EditQuestionValueEvent (setValidations validations EditQuestionValueEventData.init))
                            }
                    of
                        Just validations ->
                            [ validations ]

                        Nothing ->
                            []
                   )

        ItemSelectQuestion _ data ->
            [ Field.toHtml
                (Widgets.selectWithGroups
                    { id = fieldId uuid "listQuestion"
                    , label = gettext "List Question" config.locale
                    , value = Maybe.withDefault "" data.listQuestionUuid
                    , loose = [ ( "", gettext "Choose a list question" config.locale ) ]
                    , groups = chapterQuestionOptions config editorContext Question.isList
                    , onInput = \value -> change (EditQuestionItemSelectEvent (setListQuestionUuid (toMaybe value) EditQuestionItemSelectEventData.init))
                    }
                    |> goTo config editorContext data.listQuestionUuid (gettext "Go to list question" config.locale)
                )
            ]

        FileQuestion _ data ->
            [ Field.toHtml
                (Field.input
                    { id = fieldId uuid "fileTypes"
                    , label = gettext "File Types" config.locale
                    , value = Maybe.withDefault "" data.fileTypes
                    , onInput = \value -> change (EditQuestionFileEvent (setFileTypes (toMaybe value) EditQuestionFileEventData.init))
                    }
                    |> Field.withHintHtml (fileTypesHint config)
                )
            , Widgets.fileSize config
                { id = fieldId uuid "maxSize"
                , label = gettext "Max Size in Bytes" config.locale
                , value = Maybe.unwrap "" String.fromInt data.maxSize
                , maximum = config.maxUploadFileSize
                , onInput = \value -> change (EditQuestionFileEvent (setMaxSize (String.toInt value) EditQuestionFileEventData.init))
                }
            ]

        IntegrationQuestion _ data ->
            let
                variables =
                    KnowledgeModel.getIntegration data.integrationUuid km
                        |> Maybe.map Integration.getVariables
                        |> Maybe.withDefault []

                variableField variable =
                    Widgets.string
                        { id = fieldId uuid ("variable-" ++ variable)
                        , label = variable
                        , value = Maybe.withDefault "" (Dict.get variable data.variables)
                        , onInput =
                            \value ->
                                change
                                    (EditQuestionIntegrationEvent
                                        (setVariables (Dict.insert variable value data.variables) EditQuestionIntegrationEventData.init)
                                    )
                        }
            in
            Field.toHtml
                (Field.select
                    { id = fieldId uuid "integration"
                    , label = gettext "Integration" config.locale
                    , value = data.integrationUuid
                    , options =
                        ( "", gettext "Choose an integration" config.locale )
                            :: List.map (\i -> ( Integration.getUuid i, Integration.getVisibleName i )) (shownIntegrations editorContext km)
                    , onInput = \value -> change (EditQuestionIntegrationEvent (setIntegrationUuid value EditQuestionIntegrationEventData.init))
                    }
                    |> goTo config editorContext (toMaybe data.integrationUuid) (gettext "Go to integration" config.locale)
                )
                :: (if List.isEmpty variables then
                        []

                    else
                        [ UiForm.section
                            { id = fieldId uuid "variables"
                            , legend = gettext "Integration Configuration" config.locale
                            , content = List.map variableField variables
                            }
                        ]
                   )


{-| A way from a field that names an entity to the entity itself. It goes in
the hint, under the control, which is where `Ui.Field` puts anything that
describes what has been chosen — and which wires it into the control's
description rather than leaving it floating beside it.

Nothing while there is nothing chosen, and nothing for something that has been
deleted: a link to a deleted entity is a link to a form saying it is gone.

-}
goTo : Config msg -> EditorContext -> Maybe String -> String -> Field.Field msg -> Field.Field msg
goTo config editorContext mbUuid label field =
    case mbUuid of
        Just uuid ->
            if String.isEmpty uuid || EditorContext.isDeleted uuid editorContext then
                field

            else
                Field.withHintHtml
                    [ Link.toHtml (Link.link { url = config.entityUrl uuid, label = label }) ]
                    field

        Nothing ->
            field


{-| The integrations a question may point at: not the ones deleted in this
editor, which would be a dangling reference the moment it is published.
-}
shownIntegrations : EditorContext -> KnowledgeModel.KnowledgeModel -> List Integration
shownIntegrations editorContext km =
    List.filterNot (\integration -> EditorContext.isDeleted (Integration.getUuid integration) editorContext)
        (KnowledgeModel.getIntegrations km)


{-| Every question that passes the test, under the chapter it belongs to, in
the order the questionnaire asks them.

The chapters are numbered as the questionnaire numbers them, and keep their
number when the one above contributes nothing — the number names the chapter,
not its position in this list. A chapter with nothing to offer is left out.

-}
chapterQuestionOptions : Config msg -> EditorContext -> (Question -> Bool) -> List ( String, List ( String, String ) )
chapterQuestionOptions config editorContext wanted =
    let
        named name untitled =
            if String.isEmpty name then
                untitled

            else
                name

        group index ( chapter, questions ) =
            case
                questions
                    |> List.filterNot (\question -> EditorContext.isDeleted (Question.getUuid question) editorContext)
                    |> List.filter wanted
                    |> List.map (\question -> ( Question.getUuid question, named (Question.getTitle question) (gettext "Untitled question" config.locale) ))
            of
                [] ->
                    Nothing

                found ->
                    Just
                        ( String.fromInt (index + 1) ++ ". " ++ named chapter.title (gettext "Untitled chapter" config.locale)
                        , found
                        )
    in
    KnowledgeModel.getAllNestedQuestionsByChapter editorContext.kmEditor.knowledgeModel
        |> List.filterNot (\( chapter, _ ) -> EditorContext.isDeleted chapter.uuid editorContext)
        |> List.indexedMap group
        |> Maybe.values


{-| What may be written in File Types. The examples are code, so they are set as
code rather than quoted in the middle of a sentence.
-}
fileTypesHint : Config msg -> List (Html msg)
fileTypesHint config =
    [ Html.text (gettext "A comma separated list of extensions, mime types, or both — for example " config.locale)
    , Html.code [] [ Html.text ".xls,.xlsx" ]
    , Html.text (gettext " or " config.locale)
    , Html.code [] [ Html.text "application/pdf" ]
    , Html.text "."
    ]


allValueTypes : List QuestionValueType
allValueTypes =
    [ StringQuestionValueType
    , DateQuestionValueType
    , DateTimeQuestionValueType
    , TimeQuestionValueType
    , NumberQuestionValueType
    , TextQuestionValueType
    , EmailQuestionValueType
    , UrlQuestionValueType
    , ColorQuestionValueType
    ]


questionTypeOptions : Config msg -> List ( String, String )
questionTypeOptions config =
    [ ( "Options", gettext "Options" config.locale )
    , ( "List", gettext "List of Items" config.locale )
    , ( "Value", gettext "Value" config.locale )
    , ( "Integration", gettext "Integration" config.locale )
    , ( "MultiChoice", gettext "Multi-Choice" config.locale )
    , ( "ItemSelect", gettext "Item Select" config.locale )
    , ( "File", gettext "File" config.locale )
    ]


{-| Changing a question's kind. Everything under the old kind goes with it.
-}
questionTypeChange : String -> EditQuestionEventData
questionTypeChange value =
    case value of
        "List" ->
            EditQuestionListEvent EditQuestionListEventData.init

        "Value" ->
            EditQuestionValueEvent (setValueType StringQuestionValueType EditQuestionValueEventData.init)

        "Integration" ->
            EditQuestionIntegrationEvent EditQuestionIntegrationEventData.init

        "MultiChoice" ->
            EditQuestionMultiChoiceEvent EditQuestionMultiChoiceEventData.init

        "ItemSelect" ->
            EditQuestionItemSelectEvent EditQuestionItemSelectEventData.init

        "File" ->
            EditQuestionFileEvent EditQuestionFileEventData.init

        _ ->
            EditQuestionOptionsEvent EditQuestionOptionsEventData.init



-- THE REFERENCE


referenceForm : Config msg -> EditorContext -> Reference -> Form msg
referenceForm config editorContext reference =
    let
        uuid =
            Reference.getUuid reference

        km =
            editorContext.kmEditor.knowledgeModel

        change data =
            edit config editorContext uuid EditReferenceEvent data

        typeFields =
            case reference of
                ResourcePageReference data ->
                    [ Widgets.select
                        { id = fieldId uuid "resourcePage"
                        , label = gettext "Resource Page" config.locale
                        , value = Maybe.withDefault "" data.resourcePageUuid
                        , options =
                            ( "", gettext "Choose a resource page" config.locale )
                                :: List.map (\page -> ( page.uuid, page.title )) (KnowledgeModel.getAllResourcePages km)
                        , onInput = \value -> change (EditReferenceResourcePageEvent (setResourcePageUuid (toMaybe value) EditReferenceResourcePageEventData.init))
                        }
                    , annotationsField config editorContext uuid data.annotations (\value -> EditReferenceEvent (EditReferenceResourcePageEvent (setAnnotations value EditReferenceResourcePageEventData.init)))
                    ]

                URLReference data ->
                    [ Widgets.string
                        { id = fieldId uuid "url"
                        , label = gettext "URL" config.locale
                        , value = data.url
                        , onInput = \value -> change (EditReferenceURLEvent (setUrl value EditReferenceURLEventData.init))
                        }
                    , Widgets.string
                        { id = fieldId uuid "label"
                        , label = gettext "Label" config.locale
                        , value = data.label
                        , onInput = \value -> change (EditReferenceURLEvent (setLabel value EditReferenceURLEventData.init))
                        }
                    , annotationsField config editorContext uuid data.annotations (\value -> EditReferenceEvent (EditReferenceURLEvent (setAnnotations value EditReferenceURLEventData.init)))
                    ]

                CrossReference data ->
                    [ Widgets.select
                        { id = fieldId uuid "targetQuestion"
                        , label = gettext "Target Question" config.locale
                        , value = data.targetUuid
                        , options =
                            ( "", gettext "Choose a question" config.locale )
                                :: List.map (\q -> ( Question.getUuid q, Question.getTitle q )) (KnowledgeModel.getAllQuestions km)
                        , onInput = \value -> change (EditReferenceCrossEvent (setTargetUuid value EditReferenceCrossEventData.init))
                        }
                    , Widgets.string
                        { id = fieldId uuid "description"
                        , label = gettext "Description" config.locale
                        , value = data.description
                        , onInput = \value -> change (EditReferenceCrossEvent (setDescription value EditReferenceCrossEventData.init))
                        }

                    -- The wizard leaves a cross reference without an annotations
                    -- field although the other two kinds have one;
                    -- it is here.
                    , annotationsField config editorContext uuid data.annotations (\value -> EditReferenceEvent (EditReferenceCrossEvent (setAnnotations value EditReferenceCrossEventData.init)))
                    ]
    in
    { kind = gettext "Reference" config.locale
    , deleteEvent = Just DeleteReferenceEvent
    , fields =
        Widgets.select
            { id = fieldId uuid "referenceType"
            , label = gettext "Reference Type" config.locale
            , value = Reference.getTypeString reference
            , options =
                [ ( "ResourcePage", gettext "Resource Page" config.locale )
                , ( "URL", gettext "URL" config.locale )
                , ( "Cross", gettext "Question" config.locale )
                ]
            , onInput = \value -> structural config editorContext uuid EditReferenceEvent (referenceTypeChange value)
            }
            :: typeFields
    }


referenceTypeChange : String -> EditReferenceEventData
referenceTypeChange value =
    case value of
        "URL" ->
            EditReferenceURLEvent EditReferenceURLEventData.init

        "Cross" ->
            EditReferenceCrossEvent EditReferenceCrossEventData.init

        _ ->
            EditReferenceResourcePageEvent EditReferenceResourcePageEventData.init



-- A LIST OF CHILDREN


childrenField :
    Config msg
    -> EditorContext
    -> String
    ->
        { name : String
        , label : String
        , uuids : List String
        , nameOf : String -> String
        , onReorder : List String -> msg
        , onAdd : msg
        , addLabel : String
        }
    -> Html msg
childrenField config editorContext uuid cfg =
    let
        named childUuid =
            let
                name =
                    cfg.nameOf childUuid
            in
            if String.isEmpty name then
                gettext "Untitled" config.locale

            else
                name
    in
    Widgets.children config
        { id = fieldId uuid cfg.name
        , label = cfg.label
        , items =
            List.map
                (\childUuid ->
                    { uuid = childUuid
                    , name = named childUuid
                    , link = Link.toHtml (Link.link { url = config.entityUrl childUuid, label = named childUuid })
                    }
                )
                (EditorContext.filterDeleted editorContext cfg.uuids)
        , onReorder = cfg.onReorder
        , addLabel = cfg.addLabel
        , onAdd = cfg.onAdd
        , announcement = Nothing
        , drag = config.childDrag
        , onDrag = config.onChildDrag
        }



-- ODDS AND ENDS


toMaybe : String -> Maybe String
toMaybe value =
    if String.isEmpty value then
        Nothing

    else
        Just value


toggle : String -> List String -> List String
toggle value list =
    if List.member value list then
        List.filter ((/=) value) list

    else
        list ++ [ value ]
