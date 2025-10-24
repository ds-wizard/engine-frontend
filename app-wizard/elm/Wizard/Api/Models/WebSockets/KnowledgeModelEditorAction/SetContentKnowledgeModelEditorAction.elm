module Wizard.Api.Models.WebSockets.KnowledgeModelEditorAction.SetContentKnowledgeModelEditorAction exposing
    ( AddKnowledgeModelEditorEventData
    , SetContentKnowledgeModelEditorAction(..)
    , decoder
    , encode
    , getUuid
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.Event as Event exposing (Event)


type SetContentKnowledgeModelEditorAction
    = AddKnowledgeModelEditorEvent AddKnowledgeModelEditorEventData


type alias AddKnowledgeModelEditorEventData =
    { uuid : Uuid
    , event : Event
    }


decoder : Decoder SetContentKnowledgeModelEditorAction
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder SetContentKnowledgeModelEditorAction
decoderByType actionType =
    case actionType of
        "AddKnowledgeModelEditorEvent" ->
            D.succeed AddKnowledgeModelEditorEventData
                |> D.required "uuid" Uuid.decoder
                |> D.required "event" Event.decoder
                |> D.map AddKnowledgeModelEditorEvent

        _ ->
            D.fail <| "Unknown SetContentKnowledgeModelEditorAction: " ++ actionType


encode : SetContentKnowledgeModelEditorAction -> E.Value
encode action =
    case action of
        AddKnowledgeModelEditorEvent data ->
            E.object
                [ ( "type", E.string "AddKnowledgeModelEditorEvent" )
                , ( "uuid", Uuid.encode data.uuid )
                , ( "event", Event.encode data.event )
                ]


getUuid : SetContentKnowledgeModelEditorAction -> Uuid
getUuid action =
    case action of
        AddKnowledgeModelEditorEvent data ->
            data.uuid
