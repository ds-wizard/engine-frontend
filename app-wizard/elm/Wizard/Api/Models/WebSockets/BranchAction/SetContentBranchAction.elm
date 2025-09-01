module Wizard.Api.Models.WebSockets.BranchAction.SetContentBranchAction exposing
    ( AddBranchEventData
    , SetContentBranchAction(..)
    , decoder
    , encode
    , getUuid
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Uuid exposing (Uuid)
import Wizard.Api.Models.Event as Event exposing (Event)


type SetContentBranchAction
    = AddBranchEvent AddBranchEventData


type alias AddBranchEventData =
    { uuid : Uuid
    , event : Event
    }


decoder : Decoder SetContentBranchAction
decoder =
    D.field "type" D.string
        |> D.andThen decoderByType


decoderByType : String -> Decoder SetContentBranchAction
decoderByType actionType =
    case actionType of
        "AddBranchEvent" ->
            D.succeed AddBranchEventData
                |> D.required "uuid" Uuid.decoder
                |> D.required "event" Event.decoder
                |> D.map AddBranchEvent

        _ ->
            D.fail <| "Unknown SetContentBranchAction: " ++ actionType


encode : SetContentBranchAction -> E.Value
encode action =
    case action of
        AddBranchEvent data ->
            E.object
                [ ( "type", E.string "AddBranchEvent" )
                , ( "uuid", Uuid.encode data.uuid )
                , ( "event", Event.encode data.event )
                ]


getUuid : SetContentBranchAction -> Uuid
getUuid action =
    case action of
        AddBranchEvent data ->
            data.uuid
