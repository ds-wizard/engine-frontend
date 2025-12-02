module Wizard.Api.Models.KnowledgeModelMigration.KnowledgeModelMigrationState exposing
    ( KnowledgeModelMigrationState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Wizard.Api.Models.Event as Event exposing (Event)


type KnowledgeModelMigrationState
    = Conflict (Maybe Event)
    | Error
    | Completed
    | Running


decoder : Decoder KnowledgeModelMigrationState
decoder =
    D.field "type" D.string
        |> D.andThen
            (\str ->
                case str of
                    "ConflictKnowledgeModelMigrationState" ->
                        D.succeed Conflict
                            |> D.optional "targetEvent" (D.maybe Event.decoder) Nothing

                    "ErrorKnowledgeModelMigrationState" ->
                        D.succeed Error

                    "CompletedKnowledgeModelMigrationState" ->
                        D.succeed Completed

                    "RunningKnowledgeModelMigrationState" ->
                        D.succeed Running

                    unknownStateType ->
                        D.fail <| "Unknown migration state type: " ++ unknownStateType
            )
