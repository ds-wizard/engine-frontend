module KMEditor.Common.MigrationState exposing
    ( MigrationState
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import KMEditor.Common.MigrationStateType as MigrationStateType exposing (MigrationStateType)
import KMEditor.Common.Models.Events exposing (Event, eventDecoder)


type alias MigrationState =
    { stateType : MigrationStateType
    , targetEvent : Maybe Event
    }


decoder : Decoder MigrationState
decoder =
    D.succeed MigrationState
        |> D.required "stateType" MigrationStateType.decoder
        |> D.optional "targetEvent" (D.maybe eventDecoder) Nothing
