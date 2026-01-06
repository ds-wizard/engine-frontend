module Wizard.Api.Models.WebSockets.KnowledgeModelEditorMessage.SetContentKnowledgeModelEditorMessage exposing
    ( AddKnowledgeModelEditorWebSocketEventData
    , SetContentKnowledgeModelEditorMessage(..)
    , decoder
    , encode
    , getUuid
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.Event as Event exposing (Event)


type SetContentKnowledgeModelEditorMessage
    = AddKnowledgeModelEditorWebSocketEvent AddKnowledgeModelEditorWebSocketEventData


type alias AddKnowledgeModelEditorWebSocketEventData =
    { uuid : Uuid
    , event : Event
    }


decoder : Decoder SetContentKnowledgeModelEditorMessage
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder SetContentKnowledgeModelEditorMessage
decoderByType actionType =
    case actionType of
        "AddKnowledgeModelEditorWebSocketEvent" ->
            D.succeed AddKnowledgeModelEditorWebSocketEventData
                |> D.required "uuid" Uuid.decoder
                |> D.required "event" Event.decoder
                |> D.map AddKnowledgeModelEditorWebSocketEvent

        _ ->
            D.fail <| "Unknown SetContentKnowledgeModelEditorMessage: " ++ actionType


encode : SetContentKnowledgeModelEditorMessage -> E.Value
encode action =
    case action of
        AddKnowledgeModelEditorWebSocketEvent data ->
            E.object
                [ ( "type", E.string "AddKnowledgeModelEditorWebSocketEvent" )
                , ( "uuid", Uuid.encode data.uuid )
                , ( "event", Event.encode data.event )
                ]


getUuid : SetContentKnowledgeModelEditorMessage -> Uuid
getUuid action =
    case action of
        AddKnowledgeModelEditorWebSocketEvent data ->
            data.uuid
