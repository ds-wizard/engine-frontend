module Wizard.Api.Models.KnowledgeModelEditor.KnowledgeModelEditorState exposing
    ( KnowledgeModelEditorState(..)
    , decoder
    , isEditable
    )

import Json.Decode as D exposing (Decoder)


type KnowledgeModelEditorState
    = Default
    | Edited
    | Outdated
    | Migrating
    | Migrated


decoder : Decoder KnowledgeModelEditorState
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


isEditable : KnowledgeModelEditorState -> Bool
isEditable kmEditorState =
    not (kmEditorState == Migrating || kmEditorState == Migrated)
