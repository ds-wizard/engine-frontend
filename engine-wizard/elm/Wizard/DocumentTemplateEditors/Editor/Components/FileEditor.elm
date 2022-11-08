module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor exposing
    ( CurrentFileEditor
    , Model
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
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
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
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.AssetUploadModal as AssetUploadModal
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.FileTree as FileTree exposing (FileTree(..))
import Wizard.Ports as Ports



-- MODEL


type alias Model =
    { files : ActionResult (List DocumentTemplateFile)
    , assets : ActionResult (List DocumentTemplateAsset)
    , splitPane : SplitPane.State
    , currentFileEditor : CurrentFileEditor
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


type CurrentFileEditor
    = NoFileEditor
    | FileFileEditor DocumentTemplateFile
    | AssetFileEditor DocumentTemplateAsset


initialModel : Model
initialModel =
    { files = ActionResult.Loading
    , assets = ActionResult.Loading
    , splitPane = SplitPane.init SplitPane.Horizontal |> SplitPane.configureSplitter (SplitPane.percentage 0.2 (Just ( 0.05, 0.7 )))
    , currentFileEditor = NoFileEditor
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
    | SplitPaneMsg SplitPane.Msg
    | OpenFile Uuid String
    | OpenAsset Uuid String
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


saveMsg : Msg
saveMsg =
    Save



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        splitPaneSub =
            Sub.map SplitPaneMsg <|
                SplitPane.subscriptions model.splitPane

        addDropdownSub =
            Dropdown.subscriptions model.addDropdownState AddDropdownMsg
    in
    Sub.batch [ splitPaneSub, addDropdownSub ]



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
    case msg of
        SplitPaneMsg splitPaneMsg ->
            ( { model | splitPane = SplitPane.update splitPaneMsg model.splitPane }, Cmd.none )

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

        OpenFile uuid path ->
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
                    in
                    ( { model
                        | currentFileEditor = FileFileEditor file
                        , fileContents = fileContents
                        , selected = SelectedFile path
                      }
                    , cmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        OpenAsset uuid path ->
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
                    ( { model | selected = SelectedAsset path, currentFileEditor = AssetFileEditor asset }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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
                        DocumentTemplateDraftsApi.postFile cfg.documentTemplateId templateFile appState (cfg.wrapMsg << AddFileCompleted)
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
                    in
                    ( { model
                        | files = files
                        , addFileModalOpen = False
                        , addingFile = ActionResult.Unset
                        , fileContents = Dict.insert (Uuid.toString file.uuid) (Success "") model.fileContents
                        , selected = SelectedFile file.fileName
                        , currentFileEditor = FileFileEditor file
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
                        file =
                            case model.currentFileEditor of
                                FileFileEditor currentFile ->
                                    if currentFile.uuid == uuid then
                                        NoFileEditor

                                    else
                                        model.currentFileEditor

                                _ ->
                                    model.currentFileEditor
                    in
                    ( { model
                        | deleting = ActionResult.Unset
                        , deleteModalOpen = False
                        , currentFileEditor = file
                        , files = ActionResult.map (List.filter ((/=) uuid << .uuid)) model.files
                        , changedFiles = Set.remove (Uuid.toString uuid) model.changedFiles
                        , savingFiles = Dict.remove (Uuid.toString uuid) model.savingFiles
                        , selected = SelectedFolder ""
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
                        asset =
                            case model.currentFileEditor of
                                AssetFileEditor currentAsset ->
                                    if currentAsset.uuid == uuid then
                                        NoFileEditor

                                    else
                                        model.currentFileEditor

                                _ ->
                                    model.currentFileEditor
                    in
                    ( { model
                        | deleting = ActionResult.Unset
                        , deleteModalOpen = False
                        , currentFileEditor = asset
                        , assets = ActionResult.map (List.filter ((/=) uuid << .uuid)) model.assets
                        , changedFiles = Set.remove (Uuid.toString uuid) model.changedFiles
                        , savingFiles = Dict.remove (Uuid.toString uuid) model.savingFiles
                        , selected = SelectedFolder ""
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | deleting = ApiError.toActionResult appState "Unable to delete asset." error }
                    , getResultCmd cfg.logoutMsg result
                    )

        AssetUploadModalMsg assetUploadModalMsg ->
            let
                ( mbAsset, assetUploadModal, cmd ) =
                    AssetUploadModal.update (cfg.wrapMsg << AssetUploadModalMsg)
                        cfg.documentTemplateId
                        (getSelectedFolderPath model)
                        assetUploadModalMsg
                        appState
                        model.assetUploadModal

                assets =
                    case mbAsset of
                        Just asset ->
                            ActionResult.map ((++) [ asset ]) model.assets

                        Nothing ->
                            model.assets
            in
            ( { model | assets = assets, assetUploadModal = assetUploadModal }, cmd )



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
                { toMsg = SplitPaneMsg
                , customSplitter = Nothing
                }
    in
    div []
        [ SplitPane.view splitPaneConfig
            (viewSidebar appState model cfg.documentTemplate files assets)
            (viewFile appState model)
            model.splitPane
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
                    []

                _ ->
                    [ a
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
            (addDropdown :: actions)
        , div [ class "file-tree" ]
            [ ul [] [ viewFiles appState model fileTree ]
            ]
        ]


viewFiles : AppState -> Model -> FileTree -> Html Msg
viewFiles appState model fileTree =
    let
        changedFileIndicator fileUuid =
            if Set.member (Uuid.toString fileUuid) model.changedFiles then
                " *"

            else
                ""

        isSelected =
            FileTree.getPath SelectedFolder SelectedFile SelectedAsset fileTree == model.selected

        wrapFileName fileUuid name =
            name ++ changedFileIndicator fileUuid

        itemLi =
            li [ classList [ ( "selected", isSelected ) ] ]

        isOpenInEditor =
            case ( model.currentFileEditor, fileTree ) of
                ( FileFileEditor editorFile, File treeFile ) ->
                    editorFile.uuid == treeFile.uuid

                ( AssetFileEditor editorAsset, Asset treeAsset ) ->
                    editorAsset.uuid == treeAsset.uuid

                _ ->
                    False

        itemText =
            if isOpenInEditor then
                strong []

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
                    , itemText [ text (wrapFileName Uuid.nil folderData.name) ]
                    ]
                , children
                ]

        File fileData ->
            itemLi
                [ a [ onClick (OpenFile fileData.uuid fileData.path), dataCy "dt-editor_file-tree_file" ]
                    [ fa "far fa-file-alt"
                    , itemText [ text (wrapFileName fileData.uuid fileData.name) ]
                    ]
                ]

        Asset assetData ->
            itemLi
                [ a [ onClick (OpenAsset assetData.uuid assetData.path), dataCy "dt-editor_file-tree_asset" ]
                    [ fa "far fa-file"
                    , itemText [ text (wrapFileName assetData.uuid assetData.name) ]
                    ]
                ]


viewFile : AppState -> Model -> Html Msg
viewFile appState model =
    case model.currentFileEditor of
        AssetFileEditor asset ->
            Html.Keyed.node "div" [ class "w-100" ] <|
                [ ( Uuid.toString asset.uuid
                  , viewAssetContent appState asset
                  )
                ]

        FileFileEditor file ->
            case Dict.get (Uuid.toString file.uuid) model.fileContents of
                Just fileActionResult ->
                    case fileActionResult of
                        ActionResult.Success content ->
                            Html.Keyed.node "div" [ class "w-100 overflow-auto" ] <|
                                [ ( Uuid.toString file.uuid
                                  , CodeEditor.codeEditor
                                        [ CodeEditor.value content
                                        , CodeEditor.onChange (FileChanged file.uuid)
                                        , CodeEditor.language (CodeEditor.chooseLanguage (ContentType.getContentTypeText file.fileName))
                                        ]
                                  )
                                ]

                        ActionResult.Loading ->
                            div [ class "w-100" ]
                                [ Page.loader appState
                                ]

                        ActionResult.Error error ->
                            div [] [ text error ]

                        _ ->
                            viewEmptyEditor appState

                Nothing ->
                    viewEmptyEditor appState

        _ ->
            viewEmptyEditor appState


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
            [ button [ class "btn btn-outline-secondary me-2", onClick (SetAddFileModalOpen True) ] [ text (gettext "Create a file" appState.locale) ]
            , button [ class "btn btn-outline-secondary", onClick (AssetUploadModalMsg (AssetUploadModal.SetOpen True)) ] [ text (gettext "Upload an asset" appState.locale) ]
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
        , actionName = gettext "Add" appState.locale
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
                    [ text (gettext "Add" appState.locale) ]
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
