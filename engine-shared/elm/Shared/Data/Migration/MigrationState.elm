module Shared.Data.Migration.MigrationState exposing
    ( MigrationState
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Shared.Data.Event as Event exposing (Event)
import Shared.Data.Migration.MigrationState.MigrationStateType as MigrationStateType exposing (MigrationStateType)


type alias MigrationState =
    { stateType : MigrationStateType
    , targetEvent : Maybe Event
    }


decoder : Decoder MigrationState
decoder =
    D.succeed MigrationState
        |> D.required "stateType" MigrationStateType.decoder
        |> D.optional "targetEvent" (D.maybe Event.decoder) Nothing
