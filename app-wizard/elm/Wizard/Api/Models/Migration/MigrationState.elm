module Wizard.Api.Models.Migration.MigrationState exposing
    ( MigrationState
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.Event as Event exposing (Event)
import Wizard.Api.Models.Migration.MigrationState.MigrationStateType as MigrationStateType exposing (MigrationStateType)


type alias MigrationState =
    { stateType : MigrationStateType
    , targetEvent : Maybe Event
    }


decoder : Decoder MigrationState
decoder =
    D.succeed MigrationState
        |> D.required "stateType" MigrationStateType.decoder
        |> D.optional "targetEvent" (D.maybe Event.decoder) Nothing
