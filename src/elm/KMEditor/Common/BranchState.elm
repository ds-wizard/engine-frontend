module KMEditor.Common.BranchState exposing (BranchState(..), decoder)

import Json.Decode as D exposing (Decoder)


type BranchState
    = Default
    | Edited
    | Outdated
    | Migrating
    | Migrated


decoder : Decoder BranchState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Default" ->
                        D.succeed Default

                    "Edited" ->
                        D.succeed Edited

                    "Outdated" ->
                        D.succeed Outdated

                    "Migrating" ->
                        D.succeed Migrating

                    "Migrated" ->
                        D.succeed Migrated

                    unknownState ->
                        D.fail <| "Unknown knowledge model appState " ++ unknownState
            )
