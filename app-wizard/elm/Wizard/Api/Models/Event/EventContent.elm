module Wizard.Api.Models.Event.EventContent exposing
    ( EventContent(..)
    , decoder
    , encode
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Wizard.Api.Models.Event.AddAnswerEventData as AddAnswerEventData exposing (AddAnswerEventData)
import Wizard.Api.Models.Event.AddChapterEventData as AddChapterEventData exposing (AddChapterEventData)
import Wizard.Api.Models.Event.AddChoiceEventData as AddChoiceEventData exposing (AddChoiceEventData)
import Wizard.Api.Models.Event.AddExpertEventData as AddExpertEventData exposing (AddExpertEventData)
import Wizard.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData exposing (AddIntegrationEventData)
import Wizard.Api.Models.Event.AddKnowledgeModelEventData as AddKnowledgeModelEventData exposing (AddKnowledgeModelEventData)
import Wizard.Api.Models.Event.AddMetricEventData as AddMetricEventData exposing (AddMetricEventData)
import Wizard.Api.Models.Event.AddPhaseEventData as AddPhaseEventData exposing (AddPhaseEventData)
import Wizard.Api.Models.Event.AddQuestionEventData as AddQuestionEventData exposing (AddQuestionEventData)
import Wizard.Api.Models.Event.AddReferenceEventData as AddReferenceEventData exposing (AddReferenceEventData)
import Wizard.Api.Models.Event.AddResourceCollectionEventData as AddResourceCollectionEventData exposing (AddResourceCollectionEventData)
import Wizard.Api.Models.Event.AddResourcePageEventData as AddResourcePageEventData exposing (AddResourcePageEventData)
import Wizard.Api.Models.Event.AddTagEventData as AddTagEventData exposing (AddTagEventData)
import Wizard.Api.Models.Event.EditAnswerEventData as EditAnswerEventData exposing (EditAnswerEventData)
import Wizard.Api.Models.Event.EditChapterEventData as EditChapterEventData exposing (EditChapterEventData)
import Wizard.Api.Models.Event.EditChoiceEventData as EditChoiceEventData exposing (EditChoiceEventData)
import Wizard.Api.Models.Event.EditExpertEventData as EditExpertEventData exposing (EditExpertEventData)
import Wizard.Api.Models.Event.EditIntegrationEventData as EditIntegrationEventData exposing (EditIntegrationEventData)
import Wizard.Api.Models.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData exposing (EditKnowledgeModelEventData)
import Wizard.Api.Models.Event.EditMetricEventData as EditMetricEventData exposing (EditMetricEventData)
import Wizard.Api.Models.Event.EditPhaseEventData as EditPhaseEventData exposing (EditPhaseEventData)
import Wizard.Api.Models.Event.EditQuestionEventData as EditQuestionEventData exposing (EditQuestionEventData)
import Wizard.Api.Models.Event.EditReferenceEventData as EditReferenceEventData exposing (EditReferenceEventData)
import Wizard.Api.Models.Event.EditResourceCollectionEventData as EditResourceCollectionEventData exposing (EditResourceCollectionEventData)
import Wizard.Api.Models.Event.EditResourcePageEventData as EditResourcePageEventData exposing (EditResourcePageEventData)
import Wizard.Api.Models.Event.EditTagEventData as EditTagEventData exposing (EditTagEventData)
import Wizard.Api.Models.Event.MoveEventData as MoveEventData exposing (MoveEventData)


type EventContent
    = AddKnowledgeModelEvent AddKnowledgeModelEventData
    | EditKnowledgeModelEvent EditKnowledgeModelEventData
    | AddChapterEvent AddChapterEventData
    | EditChapterEvent EditChapterEventData
    | DeleteChapterEvent
    | AddMetricEvent AddMetricEventData
    | EditMetricEvent EditMetricEventData
    | DeleteMetricEvent
    | AddPhaseEvent AddPhaseEventData
    | EditPhaseEvent EditPhaseEventData
    | DeletePhaseEvent
    | AddTagEvent AddTagEventData
    | EditTagEvent EditTagEventData
    | DeleteTagEvent
    | AddIntegrationEvent AddIntegrationEventData
    | EditIntegrationEvent EditIntegrationEventData
    | DeleteIntegrationEvent
    | AddQuestionEvent AddQuestionEventData
    | EditQuestionEvent EditQuestionEventData
    | DeleteQuestionEvent
    | AddAnswerEvent AddAnswerEventData
    | EditAnswerEvent EditAnswerEventData
    | DeleteAnswerEvent
    | AddChoiceEvent AddChoiceEventData
    | EditChoiceEvent EditChoiceEventData
    | DeleteChoiceEvent
    | AddReferenceEvent AddReferenceEventData
    | EditReferenceEvent EditReferenceEventData
    | DeleteReferenceEvent
    | AddExpertEvent AddExpertEventData
    | EditExpertEvent EditExpertEventData
    | DeleteExpertEvent
    | AddResourceCollectionEvent AddResourceCollectionEventData
    | EditResourceCollectionEvent EditResourceCollectionEventData
    | DeleteResourceCollectionEvent
    | AddResourcePageEvent AddResourcePageEventData
    | EditResourcePageEvent EditResourcePageEventData
    | DeleteResourcePageEvent
    | MoveQuestionEvent MoveEventData
    | MoveAnswerEvent MoveEventData
    | MoveChoiceEvent MoveEventData
    | MoveReferenceEvent MoveEventData
    | MoveExpertEvent MoveEventData


decoder : Decoder EventContent
decoder =
    D.field "eventType" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder EventContent
decoderByType eventType =
    case eventType of
        "AddKnowledgeModelEvent" ->
            D.map AddKnowledgeModelEvent AddKnowledgeModelEventData.decoder

        "EditKnowledgeModelEvent" ->
            D.map EditKnowledgeModelEvent EditKnowledgeModelEventData.decoder

        "AddChapterEvent" ->
            D.map AddChapterEvent AddChapterEventData.decoder

        "EditChapterEvent" ->
            D.map EditChapterEvent EditChapterEventData.decoder

        "DeleteChapterEvent" ->
            D.succeed DeleteChapterEvent

        "AddMetricEvent" ->
            D.map AddMetricEvent AddMetricEventData.decoder

        "EditMetricEvent" ->
            D.map EditMetricEvent EditMetricEventData.decoder

        "DeleteMetricEvent" ->
            D.succeed DeleteMetricEvent

        "AddPhaseEvent" ->
            D.map AddPhaseEvent AddPhaseEventData.decoder

        "EditPhaseEvent" ->
            D.map EditPhaseEvent EditPhaseEventData.decoder

        "DeletePhaseEvent" ->
            D.succeed DeletePhaseEvent

        "AddTagEvent" ->
            D.map AddTagEvent AddTagEventData.decoder

        "EditTagEvent" ->
            D.map EditTagEvent EditTagEventData.decoder

        "DeleteTagEvent" ->
            D.succeed DeleteTagEvent

        "AddIntegrationEvent" ->
            D.map AddIntegrationEvent AddIntegrationEventData.decoder

        "EditIntegrationEvent" ->
            D.map EditIntegrationEvent EditIntegrationEventData.decoder

        "DeleteIntegrationEvent" ->
            D.succeed DeleteIntegrationEvent

        "AddQuestionEvent" ->
            D.map AddQuestionEvent AddQuestionEventData.decoder

        "EditQuestionEvent" ->
            D.map EditQuestionEvent EditQuestionEventData.decoder

        "DeleteQuestionEvent" ->
            D.succeed DeleteQuestionEvent

        "AddAnswerEvent" ->
            D.map AddAnswerEvent AddAnswerEventData.decoder

        "EditAnswerEvent" ->
            D.map EditAnswerEvent EditAnswerEventData.decoder

        "DeleteAnswerEvent" ->
            D.succeed DeleteAnswerEvent

        "AddChoiceEvent" ->
            D.map AddChoiceEvent AddChoiceEventData.decoder

        "EditChoiceEvent" ->
            D.map EditChoiceEvent EditChoiceEventData.decoder

        "DeleteChoiceEvent" ->
            D.succeed DeleteChoiceEvent

        "AddReferenceEvent" ->
            D.map AddReferenceEvent AddReferenceEventData.decoder

        "EditReferenceEvent" ->
            D.map EditReferenceEvent EditReferenceEventData.decoder

        "DeleteReferenceEvent" ->
            D.succeed DeleteReferenceEvent

        "AddExpertEvent" ->
            D.map AddExpertEvent AddExpertEventData.decoder

        "EditExpertEvent" ->
            D.map EditExpertEvent EditExpertEventData.decoder

        "DeleteExpertEvent" ->
            D.succeed DeleteExpertEvent

        "AddResourceCollectionEvent" ->
            D.map AddResourceCollectionEvent AddResourceCollectionEventData.decoder

        "EditResourceCollectionEvent" ->
            D.map EditResourceCollectionEvent EditResourceCollectionEventData.decoder

        "DeleteResourceCollectionEvent" ->
            D.succeed DeleteResourceCollectionEvent

        "AddResourcePageEvent" ->
            D.map AddResourcePageEvent AddResourcePageEventData.decoder

        "EditResourcePageEvent" ->
            D.map EditResourcePageEvent EditResourcePageEventData.decoder

        "DeleteResourcePageEvent" ->
            D.succeed DeleteResourcePageEvent

        "MoveQuestionEvent" ->
            D.map MoveQuestionEvent MoveEventData.decoder

        "MoveAnswerEvent" ->
            D.map MoveAnswerEvent MoveEventData.decoder

        "MoveChoiceEvent" ->
            D.map MoveChoiceEvent MoveEventData.decoder

        "MoveReferenceEvent" ->
            D.map MoveReferenceEvent MoveEventData.decoder

        "MoveExpertEvent" ->
            D.map MoveExpertEvent MoveEventData.decoder

        _ ->
            D.fail <| "Unknown event type: " ++ eventType


encode : EventContent -> E.Value
encode eventContent =
    E.object <|
        case eventContent of
            AddKnowledgeModelEvent eventData ->
                AddKnowledgeModelEventData.encode eventData

            EditKnowledgeModelEvent eventData ->
                EditKnowledgeModelEventData.encode eventData

            AddChapterEvent eventData ->
                AddChapterEventData.encode eventData

            EditChapterEvent eventData ->
                EditChapterEventData.encode eventData

            DeleteChapterEvent ->
                [ ( "eventType", E.string "DeleteChapterEvent" ) ]

            AddMetricEvent eventData ->
                AddMetricEventData.encode eventData

            EditMetricEvent eventData ->
                EditMetricEventData.encode eventData

            DeleteMetricEvent ->
                [ ( "eventType", E.string "DeleteMetricEvent" ) ]

            AddPhaseEvent eventData ->
                AddPhaseEventData.encode eventData

            EditPhaseEvent eventData ->
                EditPhaseEventData.encode eventData

            DeletePhaseEvent ->
                [ ( "eventType", E.string "DeletePhaseEvent" ) ]

            AddTagEvent eventData ->
                AddTagEventData.encode eventData

            EditTagEvent eventData ->
                EditTagEventData.encode eventData

            DeleteTagEvent ->
                [ ( "eventType", E.string "DeleteTagEvent" ) ]

            AddIntegrationEvent eventData ->
                AddIntegrationEventData.encode eventData

            EditIntegrationEvent eventData ->
                EditIntegrationEventData.encode eventData

            DeleteIntegrationEvent ->
                [ ( "eventType", E.string "DeleteIntegrationEvent" ) ]

            AddQuestionEvent eventData ->
                AddQuestionEventData.encode eventData

            EditQuestionEvent eventData ->
                EditQuestionEventData.encode eventData

            DeleteQuestionEvent ->
                [ ( "eventType", E.string "DeleteQuestionEvent" ) ]

            AddAnswerEvent eventData ->
                AddAnswerEventData.encode eventData

            EditAnswerEvent eventData ->
                EditAnswerEventData.encode eventData

            DeleteAnswerEvent ->
                [ ( "eventType", E.string "DeleteAnswerEvent" ) ]

            AddChoiceEvent eventData ->
                AddChoiceEventData.encode eventData

            EditChoiceEvent eventData ->
                EditChoiceEventData.encode eventData

            DeleteChoiceEvent ->
                [ ( "eventType", E.string "DeleteChoiceEvent" ) ]

            AddReferenceEvent eventData ->
                AddReferenceEventData.encode eventData

            EditReferenceEvent eventData ->
                EditReferenceEventData.encode eventData

            DeleteReferenceEvent ->
                [ ( "eventType", E.string "DeleteReferenceEvent" ) ]

            AddExpertEvent eventData ->
                AddExpertEventData.encode eventData

            EditExpertEvent eventData ->
                EditExpertEventData.encode eventData

            DeleteExpertEvent ->
                [ ( "eventType", E.string "DeleteExpertEvent" ) ]

            AddResourceCollectionEvent eventData ->
                AddResourceCollectionEventData.encode eventData

            EditResourceCollectionEvent eventData ->
                EditResourceCollectionEventData.encode eventData

            DeleteResourceCollectionEvent ->
                [ ( "eventType", E.string "DeleteResourceCollectionEvent" ) ]

            AddResourcePageEvent eventData ->
                AddResourcePageEventData.encode eventData

            EditResourcePageEvent eventData ->
                EditResourcePageEventData.encode eventData

            DeleteResourcePageEvent ->
                [ ( "eventType", E.string "DeleteResourcePageEvent" ) ]

            MoveQuestionEvent eventData ->
                MoveEventData.encode "MoveQuestionEvent" eventData

            MoveAnswerEvent eventData ->
                MoveEventData.encode "MoveAnswerEvent" eventData

            MoveChoiceEvent eventData ->
                MoveEventData.encode "MoveChoiceEvent" eventData

            MoveReferenceEvent eventData ->
                MoveEventData.encode "MoveReferenceEvent" eventData

            MoveExpertEvent eventData ->
                MoveEventData.encode "MoveExpertEvent" eventData
