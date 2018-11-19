module KMEditor.Editor.Update.Abstract exposing (AddEntityConfig, DeleteEntityConfig, GenerateEventConfig, UpdateFormConfig, addEntity, deleteEntity, updateEditor, updateForm, withGenerateEvent)

import Dict exposing (Dict)
import Form
import Form.Validate exposing (Validation)
import KMEditor.Common.Models.Events exposing (Event)
import KMEditor.Common.Models.Path exposing (Path, PathNode, getParentUuid)
import KMEditor.Editor.Models exposing (Model, addEvent, getEditorContext, insertEditor, setAlert)
import KMEditor.Editor.Models.Children as Children exposing (Children)
import KMEditor.Editor.Models.EditorContext exposing (EditorContext)
import KMEditor.Editor.Models.Editors exposing (Editor, EditorLike, EditorState(..), getNewState)
import Msgs
import Random exposing (Seed)
import Utils exposing (getUuid)


{- Add -}


type alias AddEntityConfig entity editorData =
    { newEntity : String -> entity
    , createEntityEditor : EditorContext -> Path -> EditorState -> entity -> Dict String Editor -> Dict String Editor
    , createPathNode : String -> PathNode
    , addEntity : entity -> editorData -> Editor
    }


addEntity : AddEntityConfig b (EditorLike a e o) -> Cmd Msgs.Msg -> Seed -> Model -> EditorLike a e o -> ( Seed, Model, Cmd Msgs.Msg )
addEntity cfg cmd seed model editorData =
    let
        ( newUuid, newSeed ) =
            getUuid seed

        entity =
            cfg.newEntity newUuid

        editorsWithEntity =
            cfg.createEntityEditor (getEditorContext model) (editorData.path ++ [ cfg.createPathNode editorData.uuid ]) Added entity model.editors

        newParentEditor =
            cfg.addEntity entity editorData

        newEditors =
            Dict.insert editorData.uuid newParentEditor editorsWithEntity
    in
    ( newSeed, { model | editors = newEditors, activeEditorUuid = Just newUuid }, cmd )



{- Edit -}


type alias GenerateEventConfig editorData e form =
    { isDirty : editorData -> Bool
    , formValidation : Validation e form
    , createEditor : editorData -> Editor
    , alert : String
    , createAddEvent : form -> editorData -> Seed -> ( Event, Seed )
    , createEditEvent : form -> editorData -> Seed -> ( Event, Seed )
    , updateEditorData : EditorContext -> EditorState -> form -> editorData -> editorData
    , updateEditors : Maybe (editorData -> editorData -> Dict String Editor -> Dict String Editor)
    }


withGenerateEvent : GenerateEventConfig (EditorLike a e o) e o -> Seed -> Model -> EditorLike a e o -> (Seed -> Model -> EditorLike a e o -> ( Seed, Model, Cmd Msgs.Msg )) -> ( Seed, Model, Cmd Msgs.Msg )
withGenerateEvent cfg seed model editorData callback =
    if cfg.isDirty editorData then
        case Form.getOutput editorData.form of
            Just form ->
                let
                    ( ( event, newSeed ), newState ) =
                        if editorData.editorState == Added then
                            ( cfg.createAddEvent form editorData seed
                            , AddedEdited
                            )
                        else
                            ( cfg.createEditEvent form editorData seed
                            , Edited
                            )

                    newEditorData =
                        cfg.updateEditorData (getEditorContext model) newState form editorData

                    updatedEditors =
                        case cfg.updateEditors of
                            Just updateEditors ->
                                updateEditors newEditorData editorData model.editors

                            Nothing ->
                                model.editors

                    newEditor =
                        cfg.createEditor newEditorData

                    newEditors =
                        Dict.insert editorData.uuid newEditor updatedEditors

                    newModel =
                        { model
                            | events = model.events ++ [ event ]
                            , editors = newEditors
                        }
                in
                callback newSeed newModel newEditorData

            _ ->
                let
                    newForm =
                        Form.update cfg.formValidation Form.Submit editorData.form

                    newEditorData =
                        { editorData | form = newForm }

                    newModel =
                        model
                            |> insertEditor (cfg.createEditor newEditorData)
                            |> setAlert cfg.alert
                in
                ( seed, newModel, Cmd.none )
    else
        callback seed model editorData



{- Delete -}


type alias DeleteEntityConfig editorData =
    { removeEntity : (String -> Children -> Children) -> String -> Editor -> Editor
    , createEditor : editorData -> Editor
    , createDeleteEvent : String -> Path -> Seed -> ( Event, Seed )
    }


deleteEntity : DeleteEntityConfig (EditorLike a e o) -> Seed -> Model -> String -> EditorLike a e o -> ( Seed, Model )
deleteEntity cfg seed model uuid editorData =
    if editorData.editorState == Added then
        let
            newEditors =
                Dict.remove uuid model.editors

            parentUuid =
                getParentUuid editorData.path

            editorsWithKm =
                Maybe.map (updateEditor newEditors (cfg.removeEntity Children.removeChild uuid)) parentUuid
                    |> Maybe.withDefault newEditors
        in
        ( seed, { model | editors = editorsWithKm, activeEditorUuid = parentUuid } )
    else
        let
            newEditor =
                cfg.createEditor { editorData | editorState = getNewState editorData.editorState Deleted, treeOpen = False }

            newEditors =
                Dict.insert editorData.uuid newEditor model.editors

            ( event, newSeed ) =
                cfg.createDeleteEvent editorData.uuid editorData.path seed

            events =
                model.events ++ [ event ]

            parentUuid =
                getParentUuid editorData.path

            editorsWithKm =
                Maybe.map (updateEditor newEditors (cfg.removeEntity Children.deleteChild uuid)) parentUuid
                    |> Maybe.withDefault newEditors
        in
        ( newSeed, { model | editors = editorsWithKm, events = events, activeEditorUuid = parentUuid } )



{- Update form -}


type alias UpdateFormConfig editorData e form =
    { formValidation : Validation e form
    , createEditor : editorData -> Editor
    }


updateForm : UpdateFormConfig (EditorLike a e o) e o -> Model -> Form.Msg -> EditorLike a e o -> Model
updateForm cfg model formMsg editorData =
    let
        newForm =
            Form.update cfg.formValidation formMsg editorData.form

        newEditor =
            cfg.createEditor { editorData | form = newForm }
    in
    insertEditor newEditor model



{- Utils -}


updateEditor : Dict String Editor -> (Editor -> Editor) -> String -> Dict String Editor
updateEditor editors update uuid =
    Dict.get uuid editors
        |> Maybe.map (update >> (\a -> Dict.insert uuid a editors))
        |> Maybe.withDefault editors
