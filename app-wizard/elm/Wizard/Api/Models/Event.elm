module Wizard.Api.Models.Event exposing
    ( Event
    , decoder
    , encode
    , getEntityVisibleName
    , getUuid
    , squash
    )

import Iso8601
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Time
import Wizard.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData
import Wizard.Api.Models.Event.AddQuestionEventData as AddQuestionEventData
import Wizard.Api.Models.Event.AddReferenceEventData as AddReferenceEventData
import Wizard.Api.Models.Event.EditAnswerEventData as EditAnswerEventData
import Wizard.Api.Models.Event.EditChapterEventData as EditChapterEventData
import Wizard.Api.Models.Event.EditChoiceEventData as EditChoiceEventData
import Wizard.Api.Models.Event.EditExpertEventData as EditExpertEventData
import Wizard.Api.Models.Event.EditIntegrationEventData as EditIntegrationEventData
import Wizard.Api.Models.Event.EditMetricEventData as EditMetricEventData
import Wizard.Api.Models.Event.EditPhaseEventData as EditPhaseEventData
import Wizard.Api.Models.Event.EditQuestionEventData as EditQuestionEventData
import Wizard.Api.Models.Event.EditReferenceEventData as EditReferenceEventData
import Wizard.Api.Models.Event.EditResourceCollectionEventData as EditResourceCollectionEventData
import Wizard.Api.Models.Event.EditResourcePageEventData as EditResourcePageEventData
import Wizard.Api.Models.Event.EditTagEventData as EditTagEventData
import Wizard.Api.Models.Event.EventContent as EventContent exposing (EventContent(..))
import Wizard.Api.Models.Event.EventField as EventField


type alias Event =
    { uuid : String
    , parentUuid : String
    , entityUuid : String
    , content : EventContent
    , createdAt : Time.Posix
    }


decoder : Decoder Event
decoder =
    D.succeed Event
        |> D.required "uuid" D.string
        |> D.required "parentUuid" D.string
        |> D.required "entityUuid" D.string
        |> D.required "content" EventContent.decoder
        |> D.required "createdAt" D.datetime


encode : Event -> E.Value
encode event =
    E.object
        [ ( "uuid", E.string event.uuid )
        , ( "parentUuid", E.string event.parentUuid )
        , ( "entityUuid", E.string event.entityUuid )
        , ( "content", EventContent.encode event.content )
        , ( "createdAt", Iso8601.encode event.createdAt )
        ]


getUuid : Event -> String
getUuid =
    .uuid


getEntityVisibleName : Event -> Maybe String
getEntityVisibleName event =
    case event.content of
        AddKnowledgeModelEvent _ ->
            Nothing

        EditKnowledgeModelEvent _ ->
            Nothing

        AddMetricEvent eventData ->
            Just eventData.title

        EditMetricEvent eventData ->
            EventField.getValue eventData.title

        AddPhaseEvent eventData ->
            Just eventData.title

        EditPhaseEvent eventData ->
            EventField.getValue eventData.title

        AddTagEvent eventData ->
            Just eventData.name

        EditTagEvent eventData ->
            EventField.getValue eventData.name

        AddIntegrationEvent eventData ->
            AddIntegrationEventData.getEntityVisibleName eventData

        EditIntegrationEvent eventData ->
            EditIntegrationEventData.getEntityVisibleName eventData

        AddChapterEvent eventData ->
            Just eventData.title

        EditChapterEvent eventData ->
            EventField.getValue eventData.title

        AddQuestionEvent eventData ->
            AddQuestionEventData.getEntityVisibleName eventData

        EditQuestionEvent eventData ->
            EditQuestionEventData.getEntityVisibleName eventData

        AddAnswerEvent eventData ->
            Just eventData.label

        EditAnswerEvent eventData ->
            EventField.getValue eventData.label

        AddChoiceEvent eventData ->
            Just eventData.label

        EditChoiceEvent eventData ->
            EventField.getValue eventData.label

        AddReferenceEvent eventData ->
            AddReferenceEventData.getEntityVisibleName eventData

        EditReferenceEvent eventData ->
            EditReferenceEventData.getEntityVisibleName eventData

        AddExpertEvent eventData ->
            Just eventData.name

        EditExpertEvent eventData ->
            EventField.getValue eventData.name

        AddResourceCollectionEvent eventData ->
            Just eventData.title

        EditResourceCollectionEvent eventData ->
            EventField.getValue eventData.title

        AddResourcePageEvent eventData ->
            Just eventData.title

        EditResourcePageEvent eventData ->
            EventField.getValue eventData.title

        _ ->
            Nothing


squash : Event -> Event -> Event
squash old new =
    let
        updateContent event newContent =
            { event | content = newContent }
    in
    case ( old.content, new.content ) of
        ( EditChapterEvent oldEditChapterData, EditChapterEvent newEditChapterData ) ->
            updateContent new (EditChapterEvent (EditChapterEventData.squash oldEditChapterData newEditChapterData))

        ( EditMetricEvent oldEditMetricData, EditMetricEvent newEditMetricData ) ->
            updateContent new (EditMetricEvent (EditMetricEventData.squash oldEditMetricData newEditMetricData))

        ( EditPhaseEvent oldEditPhaseData, EditPhaseEvent newEditPhaseData ) ->
            updateContent new (EditPhaseEvent (EditPhaseEventData.squash oldEditPhaseData newEditPhaseData))

        ( EditTagEvent oldEditTagData, EditTagEvent newEditTagData ) ->
            updateContent new (EditTagEvent (EditTagEventData.squash oldEditTagData newEditTagData))

        ( EditIntegrationEvent oldEditIntegrationData, EditIntegrationEvent newEditIntegrationData ) ->
            updateContent new (EditIntegrationEvent (EditIntegrationEventData.squash oldEditIntegrationData newEditIntegrationData))

        ( EditQuestionEvent oldEditQuestionData, EditQuestionEvent newEditQuestionData ) ->
            updateContent new (EditQuestionEvent (EditQuestionEventData.squash oldEditQuestionData newEditQuestionData))

        ( EditAnswerEvent oldEditAnswerData, EditAnswerEvent newEditAnswerData ) ->
            updateContent new (EditAnswerEvent (EditAnswerEventData.squash oldEditAnswerData newEditAnswerData))

        ( EditChoiceEvent oldEditChoiceData, EditChoiceEvent newEditChoiceData ) ->
            updateContent new (EditChoiceEvent (EditChoiceEventData.squash oldEditChoiceData newEditChoiceData))

        ( EditReferenceEvent oldEditReferenceData, EditReferenceEvent newEditReferenceData ) ->
            updateContent new (EditReferenceEvent (EditReferenceEventData.squash oldEditReferenceData newEditReferenceData))

        ( EditExpertEvent oldEditExpertData, EditExpertEvent newEditExpertData ) ->
            updateContent new (EditExpertEvent (EditExpertEventData.squash oldEditExpertData newEditExpertData))

        ( EditResourceCollectionEvent oldEditResourceCollectionData, EditResourceCollectionEvent newEditResourceCollectionData ) ->
            updateContent new (EditResourceCollectionEvent (EditResourceCollectionEventData.squash oldEditResourceCollectionData newEditResourceCollectionData))

        ( EditResourcePageEvent oldEditResourcePageData, EditResourcePageEvent newEditResourcePageData ) ->
            updateContent new (EditResourcePageEvent (EditResourcePageEventData.squash oldEditResourcePageData newEditResourcePageData))

        _ ->
            new
