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
                    "DefaultKnowledgeModelEditorState" ->
                        D.succeed Default

                    "EditedKnowledgeModelEditorState" ->
                        D.succeed Edited

                    "OutdatedKnowledgeModelEditorState" ->
                        D.succeed Outdated

                    "MigratingKnowledgeModelEditorState" ->
                        D.succeed Migrating

                    "MigratedKnowledgeModelEditorState" ->
                        D.succeed Migrated

                    unknownState ->
                        D.fail <| "Unknown knowledge model state " ++ unknownState
            )


isEditable : KnowledgeModelEditorState -> Bool
isEditable kmEditorState =
    not (kmEditorState == Migrating || kmEditorState == Migrated)
