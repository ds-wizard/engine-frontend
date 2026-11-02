module Common.Api.Models.Event.EventContent exposing
    ( EventContent(..)
    , decoder
    , encode
    , eventType
    )

import Common.Api.Models.Event.AddAnswerEventData as AddAnswerEventData exposing (AddAnswerEventData)
import Common.Api.Models.Event.AddChapterEventData as AddChapterEventData exposing (AddChapterEventData)
import Common.Api.Models.Event.AddChoiceEventData as AddChoiceEventData exposing (AddChoiceEventData)
import Common.Api.Models.Event.AddExpertEventData as AddExpertEventData exposing (AddExpertEventData)
import Common.Api.Models.Event.AddIntegrationEventData as AddIntegrationEventData exposing (AddIntegrationEventData)
import Common.Api.Models.Event.AddKnowledgeModelEventData as AddKnowledgeModelEventData exposing (AddKnowledgeModelEventData)
import Common.Api.Models.Event.AddMetricEventData as AddMetricEventData exposing (AddMetricEventData)
import Common.Api.Models.Event.AddPhaseEventData as AddPhaseEventData exposing (AddPhaseEventData)
import Common.Api.Models.Event.AddQuestionEventData as AddQuestionEventData exposing (AddQuestionEventData)
import Common.Api.Models.Event.AddReferenceEventData as AddReferenceEventData exposing (AddReferenceEventData)
import Common.Api.Models.Event.AddResourceCollectionEventData as AddResourceCollectionEventData exposing (AddResourceCollectionEventData)
import Common.Api.Models.Event.AddResourcePageEventData as AddResourcePageEventData exposing (AddResourcePageEventData)
import Common.Api.Models.Event.AddTagEventData as AddTagEventData exposing (AddTagEventData)
import Common.Api.Models.Event.EditAnswerEventData as EditAnswerEventData exposing (EditAnswerEventData)
import Common.Api.Models.Event.EditChapterEventData as EditChapterEventData exposing (EditChapterEventData)
import Common.Api.Models.Event.EditChoiceEventData as EditChoiceEventData exposing (EditChoiceEventData)
import Common.Api.Models.Event.EditExpertEventData as EditExpertEventData exposing (EditExpertEventData)
import Common.Api.Models.Event.EditIntegrationEventData as EditIntegrationEventData exposing (EditIntegrationEventData)
import Common.Api.Models.Event.EditKnowledgeModelEventData as EditKnowledgeModelEventData exposing (EditKnowledgeModelEventData)
import Common.Api.Models.Event.EditMetricEventData as EditMetricEventData exposing (EditMetricEventData)
import Common.Api.Models.Event.EditPhaseEventData as EditPhaseEventData exposing (EditPhaseEventData)
import Common.Api.Models.Event.EditQuestionEventData as EditQuestionEventData exposing (EditQuestionEventData)
import Common.Api.Models.Event.EditReferenceEventData as EditReferenceEventData exposing (EditReferenceEventData)
import Common.Api.Models.Event.EditResourceCollectionEventData as EditResourceCollectionEventData exposing (EditResourceCollectionEventData)
import Common.Api.Models.Event.EditResourcePageEventData as EditResourcePageEventData exposing (EditResourcePageEventData)
import Common.Api.Models.Event.EditTagEventData as EditTagEventData exposing (EditTagEventData)
import Common.Api.Models.Event.MoveEventData as MoveEventData exposing (MoveEventData)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


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


{-| The type the API knows the event by, as its `eventType` says.
-}
eventType : EventContent -> String
eventType content =
    case content of
        AddKnowledgeModelEvent _ ->
            "AddKnowledgeModelEvent"

        EditKnowledgeModelEvent _ ->
            "EditKnowledgeModelEvent"

        AddChapterEvent _ ->
            "AddChapterEvent"

        EditChapterEvent _ ->
            "EditChapterEvent"

        DeleteChapterEvent ->
            "DeleteChapterEvent"

        AddMetricEvent _ ->
            "AddMetricEvent"

        EditMetricEvent _ ->
            "EditMetricEvent"

        DeleteMetricEvent ->
            "DeleteMetricEvent"

        AddPhaseEvent _ ->
            "AddPhaseEvent"

        EditPhaseEvent _ ->
            "EditPhaseEvent"

        DeletePhaseEvent ->
            "DeletePhaseEvent"

        AddTagEvent _ ->
            "AddTagEvent"

        EditTagEvent _ ->
            "EditTagEvent"

        DeleteTagEvent ->
            "DeleteTagEvent"

        AddIntegrationEvent _ ->
            "AddIntegrationEvent"

        EditIntegrationEvent _ ->
            "EditIntegrationEvent"

        DeleteIntegrationEvent ->
            "DeleteIntegrationEvent"

        AddQuestionEvent _ ->
            "AddQuestionEvent"

        EditQuestionEvent _ ->
            "EditQuestionEvent"

        DeleteQuestionEvent ->
            "DeleteQuestionEvent"

        AddAnswerEvent _ ->
            "AddAnswerEvent"

        EditAnswerEvent _ ->
            "EditAnswerEvent"

        DeleteAnswerEvent ->
            "DeleteAnswerEvent"

        AddChoiceEvent _ ->
            "AddChoiceEvent"

        EditChoiceEvent _ ->
            "EditChoiceEvent"

        DeleteChoiceEvent ->
            "DeleteChoiceEvent"

        AddReferenceEvent _ ->
            "AddReferenceEvent"

        EditReferenceEvent _ ->
            "EditReferenceEvent"

        DeleteReferenceEvent ->
            "DeleteReferenceEvent"

        AddExpertEvent _ ->
            "AddExpertEvent"

        EditExpertEvent _ ->
            "EditExpertEvent"

        DeleteExpertEvent ->
            "DeleteExpertEvent"

        AddResourceCollectionEvent _ ->
            "AddResourceCollectionEvent"

        EditResourceCollectionEvent _ ->
            "EditResourceCollectionEvent"

        DeleteResourceCollectionEvent ->
            "DeleteResourceCollectionEvent"

        AddResourcePageEvent _ ->
            "AddResourcePageEvent"

        EditResourcePageEvent _ ->
            "EditResourcePageEvent"

        DeleteResourcePageEvent ->
            "DeleteResourcePageEvent"

        MoveQuestionEvent _ ->
            "MoveQuestionEvent"

        MoveAnswerEvent _ ->
            "MoveAnswerEvent"

        MoveChoiceEvent _ ->
            "MoveChoiceEvent"

        MoveReferenceEvent _ ->
            "MoveReferenceEvent"

        MoveExpertEvent _ ->
            "MoveExpertEvent"


decoder : Decoder EventContent
decoder =
    D.field "eventType" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder EventContent
decoderByType eventTypeName =
    case eventTypeName of
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
            D.fail <| "Unknown event type: " ++ eventTypeName


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
