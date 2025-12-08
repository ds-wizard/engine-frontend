module Wizard.Api.Models.Project.ProjectState exposing
    ( ProjectState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type ProjectState
    = Default
    | Outdated
    | Migrating


decoder : Decoder ProjectState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "DefaultProjectState" ->
                        D.succeed Default

                    "OutdatedProjectState" ->
                        D.succeed Outdated

                    "MigratingProjectState" ->
                        D.succeed Migrating

                    unknownState ->
                        D.fail <| "Unknown project state " ++ unknownState
            )
