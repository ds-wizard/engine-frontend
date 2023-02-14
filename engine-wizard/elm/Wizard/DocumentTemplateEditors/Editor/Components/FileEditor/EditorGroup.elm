module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.EditorGroup exposing
    ( EditorGroup
    , addAndOpenEditor
    , init
    , isEditorOpen
    , isEmpty
    , removeEditor
    , removeEditorByUuid
    )

import List.Extra as List
import Shared.Utils exposing (flip)
import Uuid exposing (Uuid)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.Editor as Editor exposing (Editor)


type alias EditorGroup =
    { id : Int
    , currentEditor : Editor
    , tabs : List Editor
    }


init : Int -> EditorGroup
init id =
    { id = id
    , currentEditor = Editor.Empty
    , tabs = []
    }


isEmpty : EditorGroup -> Bool
isEmpty =
    List.isEmpty << .tabs


isEditorOpen : Editor -> EditorGroup -> Bool
isEditorOpen editor group =
    List.member editor group.tabs


addAndOpenEditor : Editor -> EditorGroup -> EditorGroup
addAndOpenEditor editor group =
    let
        tabs =
            if isEditorOpen editor group then
                group.tabs

            else
                group.tabs ++ [ editor ]
    in
    { group
        | currentEditor = editor
        , tabs = tabs
    }


removeEditor : Editor -> EditorGroup -> EditorGroup
removeEditor editor group =
    let
        tabs =
            List.filter ((/=) editor) group.tabs

        currentEditor =
            if group.currentEditor == editor then
                Maybe.withDefault Editor.Empty (List.last tabs)

            else
                group.currentEditor
    in
    { group
        | currentEditor = currentEditor
        , tabs = tabs
    }


removeEditorByUuid : Uuid -> EditorGroup -> EditorGroup
removeEditorByUuid uuid group =
    let
        predicate e =
            case e of
                Editor.File file ->
                    file.uuid == uuid

                Editor.Asset asset ->
                    asset.uuid == uuid

                _ ->
                    False
    in
    List.find predicate group.tabs
        |> Maybe.map (flip removeEditor group)
        |> Maybe.withDefault group
