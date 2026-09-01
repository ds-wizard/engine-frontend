module Wizard.Api.Models.Project.KnowledgeModelProjectState exposing
    ( KnowledgeModelProjectState(..)
    , decoder
    )

import Json.Decode as D exposing (Decoder)


type KnowledgeModelProjectState
    = UpToDate
    | Outdated


decoder : Decoder KnowledgeModelProjectState
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "UpToDateKnowledgeModelProjectState" ->
                        D.succeed UpToDate

                    "OutdatedKnowledgeModelProjectState" ->
                        D.succeed Outdated

                    unknownState ->
                        D.fail <| "Unknown knowledge model project state " ++ unknownState
            )
