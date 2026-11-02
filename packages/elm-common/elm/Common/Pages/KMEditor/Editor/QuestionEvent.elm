module Common.Pages.KMEditor.Editor.QuestionEvent exposing
    ( Setters
    , annotationSetters
    , expertSetters
    , questionData
    , referenceSetters
    , requiredPhaseSetters
    , tagSetters
    , textSetters
    , titleSetters
    )

import Common.Api.Models.Event.EditEventSetters exposing (setAnnotations, setExpertUuids, setReferenceUuids, setRequiredPhaseUuid, setTagUuids, setText, setTitle)
import Common.Api.Models.Event.EditQuestionEventData exposing (EditQuestionEventData(..))
import Common.Api.Models.Event.EditQuestionFileEventData as EditQuestionFileEventData
import Common.Api.Models.Event.EditQuestionIntegrationEventData as EditQuestionIntegrationEventData
import Common.Api.Models.Event.EditQuestionItemSelectData as EditQuestionItemSelectEventData
import Common.Api.Models.Event.EditQuestionListEventData as EditQuestionListEventData
import Common.Api.Models.Event.EditQuestionMultiChoiceEventData as EditQuestionMultiChoiceEventData
import Common.Api.Models.Event.EditQuestionOptionsEventData as EditQuestionOptionsEventData
import Common.Api.Models.Event.EditQuestionValueEventData as EditQuestionValueEventData
import Common.Api.Models.KnowledgeModel.Annotation exposing (Annotation)
import Common.Api.Models.KnowledgeModel.Question exposing (Question(..))


{-| A question's edit event carries its kind, so one field of a question is
seven different constructors. Elm cannot take a setter that works on all seven
as one argument — `setTitle` is a different type against each — so a field
names its seven, once, and the forms use the named set.
-}
type alias Setters value =
    { options : value -> EditQuestionOptionsEventData.EditQuestionOptionsEventData -> EditQuestionOptionsEventData.EditQuestionOptionsEventData
    , list : value -> EditQuestionListEventData.EditQuestionListEventData -> EditQuestionListEventData.EditQuestionListEventData
    , value : value -> EditQuestionValueEventData.EditQuestionValueEventData -> EditQuestionValueEventData.EditQuestionValueEventData
    , integration : value -> EditQuestionIntegrationEventData.EditQuestionIntegrationEventData -> EditQuestionIntegrationEventData.EditQuestionIntegrationEventData
    , multiChoice : value -> EditQuestionMultiChoiceEventData.EditQuestionMultiChoiceEventData -> EditQuestionMultiChoiceEventData.EditQuestionMultiChoiceEventData
    , itemSelect : value -> EditQuestionItemSelectEventData.EditQuestionItemSelectEventData -> EditQuestionItemSelectEventData.EditQuestionItemSelectEventData
    , file : value -> EditQuestionFileEventData.EditQuestionFileEventData -> EditQuestionFileEventData.EditQuestionFileEventData
    }


questionData : Question -> Setters value -> value -> EditQuestionEventData
questionData question setters value =
    case question of
        OptionsQuestion _ _ ->
            EditQuestionOptionsEvent (setters.options value EditQuestionOptionsEventData.init)

        ListQuestion _ _ ->
            EditQuestionListEvent (setters.list value EditQuestionListEventData.init)

        ValueQuestion _ _ ->
            EditQuestionValueEvent (setters.value value EditQuestionValueEventData.init)

        IntegrationQuestion _ _ ->
            EditQuestionIntegrationEvent (setters.integration value EditQuestionIntegrationEventData.init)

        MultiChoiceQuestion _ _ ->
            EditQuestionMultiChoiceEvent (setters.multiChoice value EditQuestionMultiChoiceEventData.init)

        ItemSelectQuestion _ _ ->
            EditQuestionItemSelectEvent (setters.itemSelect value EditQuestionItemSelectEventData.init)

        FileQuestion _ _ ->
            EditQuestionFileEvent (setters.file value EditQuestionFileEventData.init)


titleSetters : Setters String
titleSetters =
    { options = setTitle, list = setTitle, value = setTitle, integration = setTitle, multiChoice = setTitle, itemSelect = setTitle, file = setTitle }


textSetters : Setters (Maybe String)
textSetters =
    { options = setText, list = setText, value = setText, integration = setText, multiChoice = setText, itemSelect = setText, file = setText }


requiredPhaseSetters : Setters (Maybe String)
requiredPhaseSetters =
    { options = setRequiredPhaseUuid, list = setRequiredPhaseUuid, value = setRequiredPhaseUuid, integration = setRequiredPhaseUuid, multiChoice = setRequiredPhaseUuid, itemSelect = setRequiredPhaseUuid, file = setRequiredPhaseUuid }


tagSetters : Setters (List String)
tagSetters =
    { options = setTagUuids, list = setTagUuids, value = setTagUuids, integration = setTagUuids, multiChoice = setTagUuids, itemSelect = setTagUuids, file = setTagUuids }


referenceSetters : Setters (List String)
referenceSetters =
    { options = setReferenceUuids, list = setReferenceUuids, value = setReferenceUuids, integration = setReferenceUuids, multiChoice = setReferenceUuids, itemSelect = setReferenceUuids, file = setReferenceUuids }


expertSetters : Setters (List String)
expertSetters =
    { options = setExpertUuids, list = setExpertUuids, value = setExpertUuids, integration = setExpertUuids, multiChoice = setExpertUuids, itemSelect = setExpertUuids, file = setExpertUuids }


annotationSetters : Setters (List Annotation)
annotationSetters =
    { options = setAnnotations, list = setAnnotations, value = setAnnotations, integration = setAnnotations, multiChoice = setAnnotations, itemSelect = setAnnotations, file = setAnnotations }
