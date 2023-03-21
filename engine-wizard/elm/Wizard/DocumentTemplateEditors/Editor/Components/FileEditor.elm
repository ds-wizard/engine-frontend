module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor exposing
    ( Model
    , Msg
    , Selected
    , UpdateConfig
    , ViewConfig
    , anyFileSaving
    , fetchData
    , filesChanged
    , initialModel
    , saveMsg
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, form, h5, i, iframe, img, input, li, span, strong, text, ul)
import Html.Attributes exposing (class, classList, href, id, src, target, value)
import Html.Events exposing (onClick, onInput, onSubmit, stopPropagationOn)
import Html.Keyed
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Data.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Shared.Data.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode, fa, faKeyClass, faSet)
import Shared.Setters exposing (setAssets, setFiles)
import Shared.Utils exposing (dispatch, flip, listFilterJust)
import SplitPane
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Common.Api exposing (applyResult, getResultCmd)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.CodeEditor as CodeEditor
import Wizard.Common.ContentType as ContentType
import Wizard.Common.Html.Attribute exposing (dataCy, tooltipLeft)
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.AssetUploadModal as AssetUploadModal
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.Editor as Editor exposing (Editor)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.EditorGroup as EditorGroup exposing (EditorGroup)
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.FileTree as FileTree exposing (FileTree(..))
import Wizard.Ports as Ports



-- MODEL


type alias Model =
    { files : ActionResult (List DocumentTemplateFile)
    , assets : ActionResult (List DocumentTemplateAsset)
    , filesSplitPane : SplitPane.State
    , editorSplitPane : SplitPane.State
    , activeEditor : Editor
    , activeGroup : Int
    , editorGroup1 : EditorGroup
    , editorGroup2 : EditorGroup
    , fileContents : Dict String (ActionResult String)
    , changedFiles : Set String
    , savingFiles : Dict String (ActionResult ())
    , newFolders : List String
    , selected : Selected
    , collapsed : Set String
    , addDropdownState : Dropdown.State
    , addFileModalOpen : Bool
    , addFileModalFileName : String
    , addingFile : ActionResult String
    , addFolderModalOpen : Bool
    , addFolderModalFolderName : String
    , deleteModalOpen : Bool
    , deleting : ActionResult String
    , assetUploadModal : AssetUploadModal.Model
    }


type Selected
    = SelectedFile String
    | SelectedAsset String
    | SelectedFolder String


initialModel : Model
initialModel =
    { files = ActionResult.Loading
    , assets = ActionResult.Loading
    , filesSplitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.2 (Just ( 0.05, 0.7 )))
    , editorSplitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.5 (Just ( 0.1, 0.9 )))
    , activeEditor = Editor.Empty
    , activeGroup = 1
    , editorGroup1 = EditorGroup.init 1
    , editorGroup2 = EditorGroup.init 2
    , fileContents = Dict.empty
    , changedFiles = Set.empty
    , savingFiles = Dict.empty
    , newFolders = []
    , selected = SelectedFolder ""
    , collapsed = Set.empty
    , addDropdownState = Dropdown.initialState
    , addFileModalOpen = False
    , addFileModalFileName = ""
    , addingFile = ActionResult.Unset
    , addFolderModalOpen = False
    , addFolderModalFolderName = ""
    , deleteModalOpen = False
    , deleting = ActionResult.Unset
    , assetUploadModal = AssetUploadModal.initialModel
    }


filesChanged : Model -> Bool
filesChanged =
    not << Set.isEmpty << .changedFiles


anyFileSaving : Model -> Bool
anyFileSaving model =
    Dict.values model.savingFiles
        |> List.any ActionResult.isLoading


getFile : Uuid -> Model -> Maybe DocumentTemplateFile
getFile uuid model =
    case model.files of
        Success files ->
            List.find ((==) uuid << .uuid) files

        _ ->
            Nothing


getFileByPath : String -> Model -> Maybe DocumentTemplateFile
getFileByPath path =
    ActionResult.unwrap Nothing (List.find ((==) path << .fileName)) << .files


getAssetByPath : String -> Model -> Maybe DocumentTemplateAsset
getAssetByPath path =
    ActionResult.unwrap Nothing (List.find ((==) path << .fileName)) << .assets


getSelectedFolderPath : Model -> String
getSelectedFolderPath model =
    let
        getFolder path =
            String.split "/" path
                |> List.init
                |> Maybe.unwrap "" (String.join "/")
    in
    case model.selected of
        SelectedFolder path ->
            path

        SelectedAsset path ->
            getFolder path

        SelectedFile path ->
            getFolder path


getSelectedName : Model -> Maybe String
getSelectedName model =
    case model.selected of
        SelectedFile path ->
            getFileByPath path model
                |> Maybe.map getFileName

        SelectedAsset path ->
            getAssetByPath path model
                |> Maybe.map getFileName

        _ ->
            Nothing


getFileName : { a | fileName : String } -> String
getFileName file =
    String.split "/" file.fileName
        |> List.last
        |> Maybe.withDefault file.fileName


updateFile : Uuid -> String -> Model -> Model
updateFile uuid content model =
    let
        uuidString =
            Uuid.toString uuid
    in
    { model
        | fileContents = Dict.insert uuidString (ActionResult.Success content) model.fileContents
        , changedFiles = Set.insert (Uuid.toString uuid) model.changedFiles
    }



-- MSG


type Msg
    = GetTemplateFilesCompleted (Result ApiError (List DocumentTemplateFile))
    | GetTemplateAssetsCompleted (Result ApiError (List DocumentTemplateAsset))
    | GetTemplateFileContentCompleted Uuid (Result ApiError String)
    | FilesSplitPaneMsg SplitPane.Msg
    | EditorSplitPaneMsg SplitPane.Msg
    | OpenFile Int Uuid String
    | OpenAsset Int Uuid String
    | SetActiveEditor Int Editor
    | MoveCurrentEditor
    | CloseTab Editor
    | FileChanged Uuid String
    | Save
    | FileSaveComplete Uuid (Result ApiError ())
    | Select Selected
    | SetOpen Bool String
    | AddDropdownMsg Dropdown.State
    | SetAddFileModalOpen Bool
    | AddFileModalInput String
    | AddFileModalSubmit
    | AddFileCompleted (Result ApiError DocumentTemplateFile)
    | SetAddFolderModalOpen Bool
    | AddFolderModalInput String
    | AddFolderModalSubmit
    | SetDeleteModalOpen Bool
    | DeleteSelected
    | DeleteSelectedFileCompleted Uuid (Result ApiError ())
    | DeleteSelectedAssetCompleted Uuid (Result ApiError ())
    | AssetUploadModalMsg AssetUploadModal.Msg
    | AddAsset DocumentTemplateAsset
    | AddFile DocumentTemplateFile


saveMsg : Msg
saveMsg =
    Save



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        filesSplitPaneSub =
            Sub.map FilesSplitPaneMsg <|
                SplitPane.subscriptions model.filesSplitPane

        editorSplitPaneSub =
            Sub.map EditorSplitPaneMsg <|
                SplitPane.subscriptions model.editorSplitPane

        addDropdownSub =
            Dropdown.subscriptions model.addDropdownState AddDropdownMsg
    in
    Sub.batch
        [ filesSplitPaneSub
        , editorSplitPaneSub
        , addDropdownSub
        ]



-- UPDATE


fetchData : String -> AppState -> Cmd Msg
fetchData documentTemplateId appState =
    Cmd.batch
        [ DocumentTemplateDraftsApi.getFiles documentTemplateId appState GetTemplateFilesCompleted
        , DocumentTemplateDraftsApi.getAssets documentTemplateId appState GetTemplateAssetsCompleted
        ]


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , documentTemplateId : String
    , onFileSavedMsg : msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    let
        openEditor : Int -> Editor -> Model -> Model
        openEditor groupId editor m =
            if EditorGroup.isEditorOpen editor m.editorGroup1 then
                { m | editorGroup1 = EditorGroup.addAndOpenEditor editor model.editorGroup1 }

            else if EditorGroup.isEditorOpen editor m.editorGroup2 then
                { m | editorGroup2 = EditorGroup.addAndOpenEditor editor model.editorGroup2 }

            else
                let
                    editorGroup1 =
                        if groupId == 1 then
                            EditorGroup.addAndOpenEditor editor model.editorGroup1

                        else
                            model.editorGroup1

                    editorGroup2 =
                        if groupId == 2 then
                            EditorGroup.addAndOpenEditor editor model.editorGroup2

                        else
                            model.editorGroup2
                in
                { m | editorGroup1 = editorGroup1, editorGroup2 = editorGroup2 }

        ensureActiveEditor : Model -> Model
        ensureActiveEditor m =
            if EditorGroup.isEditorOpen m.activeEditor m.editorGroup1 || EditorGroup.isEditorOpen m.activeEditor m.editorGroup2 then
                m

            else
                { m | activeEditor = m.editorGroup1.currentEditor }

        consolidateEditorGroups : Model -> Model
        consolidateEditorGroups m =
            ensureActiveEditor <|
                if EditorGroup.isEmpty m.editorGroup1 then
                    { m
                        | editorGroup1 = m.editorGroup2
                        , editorGroup2 = EditorGroup.init 2
                    }

                else
                    m
    in
    case msg of
        FilesSplitPaneMsg splitPaneMsg ->
            ( { model | filesSplitPane = SplitPane.update splitPaneMsg model.filesSplitPane }, Cmd.none )

        EditorSplitPaneMsg splitPaneMsg ->
            ( { model | editorSplitPane = SplitPane.update splitPaneMsg model.editorSplitPane }, Cmd.none )

        GetTemplateFilesCompleted result ->
            applyResult appState
                { setResult = setFiles
                , defaultError = gettext "Unable to get template files" appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                }

        GetTemplateAssetsCompleted result ->
            applyResult appState
                { setResult = setAssets
                , defaultError = gettext "Unable to get template assets" appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                }

        GetTemplateFileContentCompleted uuid result ->
            let
                setResult r m =
                    { m | fileContents = Dict.insert (Uuid.toString uuid) r m.fileContents }
            in
            applyResult appState
                { setResult = setResult
                , defaultError = gettext "Unable to get file content" appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                }

        OpenFile groupId uuid path ->
            case getFile uuid model of
                Just file ->
                    let
                        ( fileContents, cmd ) =
                            if Dict.member (Uuid.toString file.uuid) model.fileContents then
                                ( model.fileContents, Cmd.none )

                            else
                                ( Dict.insert (Uuid.toString uuid) ActionResult.Loading model.fileContents
                                , DocumentTemplateDraftsApi.getFileContent cfg.documentTemplateId file.uuid appState (cfg.wrapMsg << GetTemplateFileContentCompleted file.uuid)
                                )

                        editor =
                            Editor.File file
                    in
                    ( openEditor groupId
                        editor
                        { model
                            | activeEditor = editor
                            , fileContents = fileContents
                            , selected = SelectedFile path
                        }
                    , cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        OpenAsset groupId uuid path ->
            let
                mbAsset =
                    case model.assets of
                        Success files ->
                            List.find ((==) uuid << .uuid) files

                        _ ->
                            Nothing
            in
            case mbAsset of
                Just asset ->
                    let
                        editor =
                            Editor.Asset asset
                    in
                    ( openEditor groupId
                        editor
                        { model
                            | selected = SelectedAsset path
                            , activeEditor = editor
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        SetActiveEditor groupId editor ->
            let
                selected =
                    case editor of
                        Editor.File file ->
                            SelectedFile file.fileName

                        Editor.Asset asset ->
                            SelectedAsset asset.fileName

                        _ ->
                            model.selected
            in
            ( { model
                | activeEditor = editor
                , activeGroup = groupId
                , selected = selected
              }
            , Cmd.none
            )

        MoveCurrentEditor ->
            let
                ( editorGroup1, editorGroup2 ) =
                    if EditorGroup.isEditorOpen model.activeEditor model.editorGroup1 then
                        ( EditorGroup.removeEditor model.activeEditor model.editorGroup1
                        , EditorGroup.addAndOpenEditor model.activeEditor model.editorGroup2
                        )

                    else if EditorGroup.isEditorOpen model.activeEditor model.editorGroup2 then
                        ( EditorGroup.addAndOpenEditor model.activeEditor model.editorGroup1
                        , EditorGroup.removeEditor model.activeEditor model.editorGroup2
                        )

                    else
                        ( model.editorGroup1, model.editorGroup2 )

                activeGroup =
                    if model.activeGroup == 1 then
                        2

                    else
                        1
            in
            ( consolidateEditorGroups
                { model
                    | editorGroup1 = editorGroup1
                    , editorGroup2 = editorGroup2
                    , activeGroup = activeGroup
                }
            , Cmd.none
            )

        CloseTab editor ->
            ( consolidateEditorGroups
                { model
                    | editorGroup1 = EditorGroup.removeEditor editor model.editorGroup1
                    , editorGroup2 = EditorGroup.removeEditor editor model.editorGroup2
                }
            , Cmd.none
            )

        FileChanged uuid content ->
            ( updateFile uuid content model, Cmd.none )

        Save ->
            let
                ( savingFiles, fileCmds ) =
                    if filesChanged model then
                        let
                            toCmd ( fileUuidString, fileContent ) =
                                let
                                    fileUuid =
                                        Uuid.fromUuidString fileUuidString
                                in
                                DocumentTemplateDraftsApi.putFileContent
                                    cfg.documentTemplateId
                                    fileUuid
                                    fileContent
                                    appState
                                    (cfg.wrapMsg << FileSaveComplete fileUuid)

                            toActionResult ( fileUuidString, _ ) =
                                ( fileUuidString, ActionResult.Loading )

                            fileWithContent fileUuidString =
                                case Maybe.andThen ActionResult.toMaybe (Dict.get fileUuidString model.fileContents) of
                                    Just fileContent ->
                                        Just ( fileUuidString, fileContent )

                                    Nothing ->
                                        Nothing

                            files =
                                Set.toList model.changedFiles
                                    |> List.map fileWithContent
                                    |> listFilterJust
                        in
                        ( Dict.fromList <| List.map toActionResult files
                        , List.map toCmd files
                        )

                    else
                        ( model.savingFiles, [] )
            in
            ( { model | savingFiles = savingFiles }
            , Cmd.batch fileCmds
            )

        FileSaveComplete uuid result ->
            let
                ( actionResult, changedFiles, cmd ) =
                    case result of
                        Ok _ ->
                            ( ActionResult.Success ()
                            , Set.remove (Uuid.toString uuid) model.changedFiles
                            , dispatch cfg.onFileSavedMsg
                            )

                        Err error ->
                            ( ApiError.toActionResult appState (gettext "Unable to save file" appState.locale) error
                            , model.changedFiles
                            , getResultCmd cfg.logoutMsg result
                            )
            in
            ( { model
                | savingFiles = Dict.insert (Uuid.toString uuid) actionResult model.savingFiles
                , changedFiles = changedFiles
              }
            , cmd
            )

        Select fileTree ->
            ( { model | selected = fileTree }, Cmd.none )

        SetOpen open path ->
            if open then
                ( { model | collapsed = Set.remove path model.collapsed }, Cmd.none )

            else
                ( { model | collapsed = Set.insert path model.collapsed }, Cmd.none )

        AddDropdownMsg dropdownState ->
            ( { model | addDropdownState = dropdownState }, Cmd.none )

        SetAddFileModalOpen open ->
            ( { model | addFileModalOpen = open, addFileModalFileName = "" }, Ports.focus "#file-name" )

        AddFileModalInput input ->
            ( { model | addFileModalFileName = input }, Cmd.none )

        AddFileModalSubmit ->
            if String.isEmpty model.addFileModalFileName then
                ( model, Cmd.none )

            else
                let
                    fileName =
                        String.join "/" (List.filter (not << String.isEmpty) [ getSelectedFolderPath model, model.addFileModalFileName ])

                    templateFile =
                        { uuid = Uuid.nil
                        , fileName = fileName
                        }

                    cmd =
                        DocumentTemplateDraftsApi.postFile cfg.documentTemplateId templateFile "" appState (cfg.wrapMsg << AddFileCompleted)
                in
                ( { model | addingFile = ActionResult.Loading }
                , cmd
                )

        AddFileCompleted result ->
            case result of
                Ok file ->
                    let
                        files =
                            ActionResult.map ((++) [ file ]) model.files

                        editor =
                            Editor.File file
                    in
                    ( { model
                        | files = files
                        , addFileModalOpen = False
                        , addingFile = ActionResult.Unset
                        , fileContents = Dict.insert (Uuid.toString file.uuid) (Success "") model.fileContents
                        , selected = SelectedFile file.fileName
                        , activeEditor = editor
                        , editorGroup1 = EditorGroup.addAndOpenEditor editor model.editorGroup1
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | addingFile = ApiError.toActionResult appState "Unable to create file." error }
                    , getResultCmd cfg.logoutMsg result
                    )

        SetAddFolderModalOpen open ->
            ( { model | addFolderModalOpen = open, addFolderModalFolderName = "" }, Ports.focus "#folder-name" )

        AddFolderModalInput input ->
            ( { model | addFolderModalFolderName = input }, Cmd.none )

        AddFolderModalSubmit ->
            let
                folder =
                    String.join "/" (List.filter (not << String.isEmpty) [ getSelectedFolderPath model, model.addFolderModalFolderName ])
            in
            ( { model
                | newFolders = folder :: model.newFolders
                , addFolderModalOpen = False
                , selected = SelectedFolder folder
              }
            , Cmd.none
            )

        SetDeleteModalOpen open ->
            ( { model | deleteModalOpen = open }, Cmd.none )

        DeleteSelected ->
            case model.selected of
                SelectedFile path ->
                    case getFileByPath path model of
                        Just file ->
                            ( { model
                                | deleting = ActionResult.Loading
                              }
                            , DocumentTemplateDraftsApi.deleteFile cfg.documentTemplateId file.uuid appState (cfg.wrapMsg << DeleteSelectedFileCompleted file.uuid)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                SelectedAsset path ->
                    case getAssetByPath path model of
                        Just asset ->
                            ( { model
                                | deleting = ActionResult.Loading
                              }
                            , DocumentTemplateDraftsApi.deleteAsset cfg.documentTemplateId asset.uuid appState (cfg.wrapMsg << DeleteSelectedAssetCompleted asset.uuid)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                SelectedFolder _ ->
                    ( model, Cmd.none )

        DeleteSelectedFileCompleted uuid result ->
            case result of
                Ok _ ->
                    let
                        editorGroup1 =
                            EditorGroup.removeEditorByUuid uuid model.editorGroup1

                        editorGroup2 =
                            EditorGroup.removeEditorByUuid uuid model.editorGroup2

                        file =
                            case model.activeEditor of
                                Editor.File currentFile ->
                                    if currentFile.uuid == uuid then
                                        editorGroup1.currentEditor

                                    else
                                        model.activeEditor

                                _ ->
                                    model.activeEditor
                    in
                    ( consolidateEditorGroups
                        { model
                            | deleting = ActionResult.Unset
                            , deleteModalOpen = False
                            , activeEditor = file
                            , files = ActionResult.map (List.filter ((/=) uuid << .uuid)) model.files
                            , changedFiles = Set.remove (Uuid.toString uuid) model.changedFiles
                            , savingFiles = Dict.remove (Uuid.toString uuid) model.savingFiles
                            , selected = SelectedFolder ""
                            , editorGroup1 = editorGroup1
                            , editorGroup2 = editorGroup2
                        }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | deleting = ApiError.toActionResult appState "Unable to delete file." error }
                    , getResultCmd cfg.logoutMsg result
                    )

        DeleteSelectedAssetCompleted uuid result ->
            case result of
                Ok _ ->
                    let
                        editorGroup1 =
                            EditorGroup.removeEditorByUuid uuid model.editorGroup1

                        editorGroup2 =
                            EditorGroup.removeEditorByUuid uuid model.editorGroup2

                        asset =
                            case model.activeEditor of
                                Editor.Asset currentAsset ->
                                    if currentAsset.uuid == uuid then
                                        editorGroup1.currentEditor

                                    else
                                        model.activeEditor

                                _ ->
                                    model.activeEditor
                    in
                    ( consolidateEditorGroups
                        { model
                            | deleting = ActionResult.Unset
                            , deleteModalOpen = False
                            , activeEditor = asset
                            , assets = ActionResult.map (List.filter ((/=) uuid << .uuid)) model.assets
                            , changedFiles = Set.remove (Uuid.toString uuid) model.changedFiles
                            , savingFiles = Dict.remove (Uuid.toString uuid) model.savingFiles
                            , selected = SelectedFolder ""
                            , editorGroup1 = editorGroup1
                            , editorGroup2 = editorGroup2
                        }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | deleting = ApiError.toActionResult appState "Unable to delete asset." error }
                    , getResultCmd cfg.logoutMsg result
                    )

        AssetUploadModalMsg assetUploadModalMsg ->
            let
                assetUploadModalConfig =
                    { wrapMsg = cfg.wrapMsg << AssetUploadModalMsg
                    , addAssetMsg = cfg.wrapMsg << AddAsset
                    , addFileMsg = cfg.wrapMsg << AddFile
                    , documentTemplateId = cfg.documentTemplateId
                    , path = getSelectedFolderPath model
                    }

                ( assetUploadModal, cmd ) =
                    AssetUploadModal.update assetUploadModalConfig
                        assetUploadModalMsg
                        appState
                        model.assetUploadModal
            in
            ( { model | assetUploadModal = assetUploadModal }, cmd )

        AddAsset asset ->
            ( { model | assets = ActionResult.map ((++) [ asset ]) model.assets }, Cmd.none )

        AddFile file ->
            ( { model | files = ActionResult.map ((++) [ file ]) model.files }, Cmd.none )



-- VIEW


type alias ViewConfig =
    { documentTemplate : DocumentTemplateDraftDetail }


view : ViewConfig -> AppState -> Model -> Html Msg
view cfg appState model =
    Page.actionResultView appState (viewFileEditor cfg appState model) (ActionResult.combine model.files model.assets)


viewFileEditor : ViewConfig -> AppState -> Model -> ( List DocumentTemplateFile, List DocumentTemplateAsset ) -> Html Msg
viewFileEditor cfg appState model ( files, assets ) =
    let
        splitPaneConfig =
            SplitPane.createViewConfig
                { toMsg = FilesSplitPaneMsg
                , customSplitter = Nothing
                }
    in
    div []
        [ SplitPane.view splitPaneConfig
            (viewSidebar appState model cfg.documentTemplate files assets)
            (viewEditorContent appState model)
            model.filesSplitPane
        , viewAddFileModal appState model
        , viewAddFolderModal appState model
        , viewDeleteModal appState model
        , Html.map AssetUploadModalMsg <| AssetUploadModal.view appState model.assetUploadModal
        ]


viewSidebar : AppState -> Model -> DocumentTemplateDraftDetail -> List DocumentTemplateFile -> List DocumentTemplateAsset -> Html Msg
viewSidebar appState model documentTemplate files assets =
    let
        fileTree =
            buildFileTree documentTemplate files assets model.newFolders

        addDropdown =
            Dropdown.dropdown model.addDropdownState
                { options = []
                , toggleMsg = AddDropdownMsg
                , toggleButton =
                    Dropdown.toggle
                        [ Button.roleLink
                        , Button.attrs
                            [ class "with-icon"
                            , dataCy "dt-editor_file-tree_add"
                            ]
                        ]
                        [ fa "fas fa-plus", text (gettext "Add" appState.locale) ]
                , items =
                    [ Dropdown.buttonItem
                        [ class "dropdown-item-icon"
                        , dataCy "dt-editor_file-tree_add-folder"
                        , onClick (SetAddFolderModalOpen True)
                        ]
                        [ fa "fas fa-folder", text (gettext "Folder" appState.locale) ]
                    , Dropdown.buttonItem
                        [ class "dropdown-item-icon"
                        , dataCy "dt-editor_file-tree_add-file"
                        , onClick (SetAddFileModalOpen True)
                        ]
                        [ fa "far fa-file-alt", text (gettext "File" appState.locale) ]
                    , Dropdown.divider
                    , Dropdown.buttonItem
                        [ class "dropdown-item-icon"
                        , dataCy "dt-editor_file-tree_upload"
                        , onClick (AssetUploadModalMsg (AssetUploadModal.SetOpen True))
                        ]
                        [ fa "fas fa-upload", text (gettext "Upload" appState.locale) ]
                    ]
                }

        actions =
            case model.selected of
                SelectedFolder _ ->
                    emptyNode

                _ ->
                    let
                        groupLink =
                            if List.length model.editorGroup1.tabs + List.length model.editorGroup2.tabs > 1 then
                                a
                                    ([ onClick MoveCurrentEditor, class "me-3" ]
                                        ++ tooltipLeft (gettext "Move to opposite group" appState.locale)
                                    )
                                    [ fa "fas fa-columns" ]

                            else
                                emptyNode
                    in
                    span []
                        [ groupLink
                        , a
                            ([ class "text-danger"
                             , onClick (SetDeleteModalOpen True)
                             , dataCy "dt-editor_file-tree_delete"
                             ]
                                ++ tooltipLeft (gettext "Delete" appState.locale)
                            )
                            [ faSet "_global.delete" appState ]
                        ]
    in
    div [ class "w-100 d-flex flex-column" ]
        [ div [ class "file-tree-actions bg-light p-2 d-flex justify-content-between align-items-center" ]
            [ addDropdown, actions ]
        , div [ class "file-tree" ]
            [ ul [] [ viewFiles appState model fileTree ]
            ]
        ]


viewFiles : AppState -> Model -> FileTree -> Html Msg
viewFiles appState model fileTree =
    let
        isSelected =
            FileTree.getPath SelectedFolder SelectedFile SelectedAsset fileTree == model.selected

        itemLi =
            li [ classList [ ( "selected", isSelected ) ] ]

        isOpenInEditor =
            case ( model.activeEditor, fileTree ) of
                ( Editor.File editorFile, File treeFile ) ->
                    editorFile.uuid == treeFile.uuid

                ( Editor.Asset editorAsset, Asset treeAsset ) ->
                    editorAsset.uuid == treeAsset.uuid

                _ ->
                    False

        itemText =
            if isOpenInEditor then
                span [ class "active" ]

            else
                span []
    in
    case fileTree of
        Folder folderData ->
            let
                icon =
                    if folderData.isRoot then
                        fa "fas fa-file-invoice"

                    else
                        fa "fas fa-folder"

                isOpen =
                    not (Set.member folderData.path model.collapsed)

                children =
                    if isOpen then
                        ul [] (List.map (viewFiles appState model) (List.sortWith FileTree.compare folderData.children))

                    else
                        emptyNode
            in
            itemLi
                [ a [ class "caret", onClick (SetOpen (not isOpen) folderData.path) ]
                    [ i
                        [ classList
                            [ ( faKeyClass "kmEditor.treeClosed" appState, not isOpen )
                            , ( faKeyClass "kmEditor.treeOpened" appState, isOpen )
                            ]
                        ]
                        []
                    ]
                , a [ onClick (Select (SelectedFolder folderData.path)), dataCy "dt-editor_file-tree_folder" ]
                    [ icon
                    , itemText [ text (wrapFileName model Uuid.nil folderData.name) ]
                    ]
                , children
                ]

        File fileData ->
            itemLi
                [ a [ onClick (OpenFile model.activeGroup fileData.uuid fileData.path), dataCy "dt-editor_file-tree_file" ]
                    [ fa "far fa-file-alt"
                    , itemText [ text (wrapFileName model fileData.uuid fileData.name) ]
                    ]
                ]

        Asset assetData ->
            itemLi
                [ a [ onClick (OpenAsset model.activeGroup assetData.uuid assetData.path), dataCy "dt-editor_file-tree_asset" ]
                    [ fa "far fa-file"
                    , itemText [ text (wrapFileName model assetData.uuid assetData.name) ]
                    ]
                ]


wrapFileName : Model -> Uuid -> String -> String
wrapFileName model fileUuid name =
    if Set.member (Uuid.toString fileUuid) model.changedFiles then
        name ++ " *"

    else
        name


viewEditorContent : AppState -> Model -> Html Msg
viewEditorContent appState model =
    if EditorGroup.isEmpty model.editorGroup2 then
        viewEditorGroup appState model model.editorGroup1

    else
        let
            splitPaneConfig =
                SplitPane.createViewConfig
                    { toMsg = EditorSplitPaneMsg
                    , customSplitter = Nothing
                    }
        in
        SplitPane.view splitPaneConfig
            (viewEditorGroup appState model model.editorGroup1)
            (viewEditorGroup appState model model.editorGroup2)
            model.editorSplitPane


viewEditorGroup : AppState -> Model -> EditorGroup -> Html Msg
viewEditorGroup appState model editorGroup =
    let
        editorContent =
            case editorGroup.currentEditor of
                Editor.Asset asset ->
                    Html.Keyed.node "div" [ class "w-100" ] <|
                        [ ( Uuid.toString asset.uuid
                          , viewAssetContent appState asset
                          )
                        ]

                Editor.File file ->
                    case Dict.get (Uuid.toString file.uuid) model.fileContents of
                        Just fileActionResult ->
                            case fileActionResult of
                                ActionResult.Success content ->
                                    Html.Keyed.node "div" [ class "w-100 overflow-auto" ] <|
                                        [ ( Uuid.toString file.uuid
                                          , CodeEditor.codeEditor
                                                [ CodeEditor.value content
                                                , CodeEditor.onChange (FileChanged file.uuid)
                                                , CodeEditor.onFocus (SetActiveEditor editorGroup.id editorGroup.currentEditor)
                                                , CodeEditor.language (CodeEditor.chooseLanguage (ContentType.getContentTypeText file.fileName))
                                                ]
                                          )
                                        ]

                                ActionResult.Loading ->
                                    div [ class "w-100" ]
                                        [ Page.loader appState
                                        ]

                                ActionResult.Error error ->
                                    div [ class "m-3" ]
                                        [ Flash.error appState error ]

                                _ ->
                                    viewEmptyEditor appState

                        Nothing ->
                            viewEmptyEditor appState

                _ ->
                    viewEmptyEditor appState
    in
    div [ class "DocumentTemplateEditor__FileEditor" ]
        [ viewTabs appState model editorGroup
        , editorContent
        ]


viewTabs : AppState -> Model -> EditorGroup -> Html Msg
viewTabs appState model editorGroup =
    let
        viewTab openMsg tab file =
            span
                [ onClick (openMsg editorGroup.id file.uuid file.fileName)
                , class "tab"
                , classList
                    [ ( "active", model.activeEditor == tab )
                    , ( "active-group", editorGroup.currentEditor == tab )
                    ]
                ]
                [ text (wrapFileName model file.uuid (getFileName file))
                , span
                    [ class "ms-2 tab-close"
                    , stopPropagationOn "click" (D.succeed ( CloseTab tab, True ))
                    ]
                    [ faSet "_global.close" appState ]
                ]

        viewTabWrapper tab =
            case tab of
                Editor.File file ->
                    viewTab OpenFile tab file

                Editor.Asset asset ->
                    viewTab OpenAsset tab asset

                _ ->
                    emptyNode
    in
    div [ class "tabs" ] (List.map viewTabWrapper editorGroup.tabs)


viewAssetContent : AppState -> DocumentTemplateAsset -> Html Msg
viewAssetContent appState asset =
    if ContentType.isImage asset.contentType then
        div [ class "DocumentTemplateEditor__Asset DocumentTemplateEditor__Asset--Image" ] [ img [ src asset.url ] [] ]

    else if asset.contentType == "application/pdf" && appState.navigator.pdf then
        div [ class "DocumentTemplateEditor__Asset DocumentTemplateEditor__Asset--Pdf" ] [ iframe [ src asset.url ] [] ]

    else
        let
            fileName =
                String.split "/" asset.fileName
                    |> List.last
                    |> Maybe.withDefault asset.fileName
        in
        div [ class "DocumentTemplateEditor__Asset DocumentTemplateEditor__Asset--Other" ]
            [ div [ class "d-flex" ]
                [ div [ class "icon" ] [ fa "far fa-file" ] ]
            , div []
                [ div [ class "filename" ] [ text fileName ]
                , a [ class "btn btn-outline-secondary with-icon", href asset.url, target "_blank" ]
                    [ faSet "_global.download" appState
                    , text (gettext "Download" appState.locale)
                    ]
                ]
            ]


viewEmptyEditor : AppState -> Html Msg
viewEmptyEditor appState =
    div [ class "DocumentTemplateEditor__EmptyEditor" ]
        [ div [] [ strong [] [ text (gettext "Open a file from the file tree" appState.locale) ] ]
        , div [ class "mt-2 mb-3" ] [ text (gettext "or" appState.locale) ]
        , div []
            [ button [ class "btn btn-outline-secondary me-2", onClick (SetAddFileModalOpen True) ] [ text (gettext "Create new" appState.locale) ]
            , button [ class "btn btn-outline-secondary", onClick (AssetUploadModalMsg (AssetUploadModal.SetOpen True)) ] [ text (gettext "Upload" appState.locale) ]
            ]
        ]


viewAddFileModal : AppState -> Model -> Html Msg
viewAddFileModal appState model =
    let
        modalContent =
            [ form [ onSubmit AddFileModalSubmit ]
                [ input
                    [ class "form-control"
                    , id "file-name"
                    , value model.addFileModalFileName
                    , onInput AddFileModalInput
                    ]
                    []
                ]
            ]
    in
    Modal.confirm appState
        { modalTitle = gettext "New file" appState.locale
        , modalContent = modalContent
        , visible = model.addFileModalOpen
        , actionResult = model.addingFile
        , actionName = gettext "Add file" appState.locale
        , actionMsg = AddFileModalSubmit
        , cancelMsg = Just (SetAddFileModalOpen False)
        , dangerous = False
        , dataCy = "add-file-modal"
        }


viewAddFolderModal : AppState -> Model -> Html Msg
viewAddFolderModal appState model =
    let
        modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "New folder" appState.locale) ] ]
            , form [ class "modal-body", onSubmit AddFolderModalSubmit ]
                [ input
                    [ class "form-control"
                    , id "folder-name"
                    , value model.addFolderModalFolderName
                    , onInput AddFolderModalInput
                    ]
                    []
                ]
            , div [ class "modal-footer" ]
                [ button
                    [ onClick AddFolderModalSubmit
                    , class "btn btn-primary"
                    , dataCy "modal_action-button"
                    ]
                    [ text (gettext "Add folder" appState.locale) ]
                , button
                    [ onClick (SetAddFolderModalOpen False)
                    , class "btn btn-secondary"
                    ]
                    [ text (gettext "Cancel" appState.locale) ]
                ]
            ]
    in
    Modal.simple
        { modalContent = modalContent
        , visible = model.addFolderModalOpen
        , dataCy = "add-file-modal"
        }


viewDeleteModal : AppState -> Model -> Html Msg
viewDeleteModal appState model =
    let
        fileName =
            getSelectedName model
    in
    Modal.confirm appState
        { modalTitle = "Delete"
        , modalContent =
            String.formatHtml (gettext "Are you sure you want to permanently delete %s?" appState.locale)
                [ strong [] [ text (Maybe.withDefault "" fileName) ] ]
        , visible = model.deleteModalOpen && Maybe.isJust fileName
        , actionResult = model.deleting
        , actionName = gettext "Delete" appState.locale
        , actionMsg = DeleteSelected
        , cancelMsg = Just (SetDeleteModalOpen False)
        , dangerous = True
        , dataCy = "document-template-editor_delete-modal"
        }


buildFileTree : DocumentTemplateDraftDetail -> List DocumentTemplateFile -> List DocumentTemplateAsset -> List String -> FileTree
buildFileTree template files assets newFolders =
    FileTree.root template.name
        |> flip (List.foldl FileTree.addFolder) newFolders
        |> flip (List.foldl FileTree.addFile) (List.map (\a -> ( a.uuid, a.fileName )) files)
        |> flip (List.foldl FileTree.addAsset) (List.map (\a -> ( a.uuid, a.fileName )) assets)
