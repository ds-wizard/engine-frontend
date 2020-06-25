module Shared.Data.Migration.MigrationState.MigrationStateType exposing
    ( MigrationStateType(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type MigrationStateType
    = ConflictState
    | ErrorState
    | CompletedState
    | RunningState


decoder : Decoder MigrationStateType
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "ConflictState" ->
                        D.succeed ConflictState

                    "ErrorState" ->
                        D.succeed ErrorState

                    "CompletedState" ->
                        D.succeed CompletedState

                    "RunningState" ->
                        D.succeed RunningState

                    unknownStateType ->
                        D.fail <| "Unknown migration state type: " ++ unknownStateType
            )
