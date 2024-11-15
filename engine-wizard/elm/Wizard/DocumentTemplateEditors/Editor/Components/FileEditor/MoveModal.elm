module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.MoveModal exposing
    ( Model
    , MoveModalState
    , Msg
    , UpdateConfig
    , initialModel
    , openAsset
    , openFile
    , openFolder
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, li, span, strong, text, ul)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import List.Extra as List
import Registry.Components.FontAwesome exposing (fas)
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Data.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Shared.Data.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (emptyNode)
import Shared.Utils exposing (dispatch)
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.Modal as Modal
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.FileTree as FileTree exposing (FileTree)


type alias Model =
    { state : MoveModalState
    , moving : ActionResult String
    , selected : Maybe String
    }


type MoveModalState
    = Closed
    | MovingFile DocumentTemplateFile
    | MovingAsset DocumentTemplateAsset
    | MovingFolder String


initialModel : Model
initialModel =
    { state = Closed
    , moving = ActionResult.Unset
    , selected = Nothing
    }


type Msg
    = SetState MoveModalState
    | SetSelected String
    | MoveSubmit
    | MoveCompleted String (Result ApiError ())


openFile : DocumentTemplateFile -> Msg
openFile file =
    SetState (MovingFile file)


openAsset : DocumentTemplateAsset -> Msg
openAsset asset =
    SetState (MovingAsset asset)


openFolder : String -> Msg
openFolder path =
    SetState (MovingFolder path)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , documentTemplateId : String
    , fileContents : Dict String (ActionResult String)
    , onRenameFile : Uuid -> String -> msg
    , onRenameAsset : Uuid -> String -> msg
    , onRenameFolder : String -> String -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        SetState state ->
            ( { model
                | state = state
                , moving = ActionResult.Unset
                , selected = Nothing
              }
            , Cmd.none
            )

        SetSelected selected ->
            ( { model | selected = Just selected }
            , Cmd.none
            )

        MoveSubmit ->
            case model.selected of
                Just path ->
                    let
                        parts =
                            if String.isEmpty path then
                                []

                            else
                                String.split "/" path
                    in
                    case model.state of
                        MovingFile file ->
                            let
                                fileName =
                                    String.join "/" (parts ++ [ getNameFromPath file.fileName ])

                                fileContent =
                                    Dict.get (Uuid.toString file.uuid) cfg.fileContents
                                        |> Maybe.withDefault (ActionResult.Success "")
                                        |> ActionResult.withDefault ""

                                templateFile =
                                    { file | fileName = fileName }

                                cmd =
                                    DocumentTemplateDraftsApi.putFile cfg.documentTemplateId templateFile fileContent appState (cfg.wrapMsg << MoveCompleted fileName)
                            in
                            ( { model | moving = ActionResult.Loading }, cmd )

                        MovingAsset asset ->
                            let
                                assetName =
                                    String.join "/" (parts ++ [ getNameFromPath asset.fileName ])

                                templateAsset =
                                    { asset | fileName = assetName }

                                cmd =
                                    DocumentTemplateDraftsApi.putAsset cfg.documentTemplateId templateAsset appState (cfg.wrapMsg << MoveCompleted assetName)
                            in
                            ( { model | moving = ActionResult.Loading }, cmd )

                        MovingFolder currentPath ->
                            let
                                newPath =
                                    String.join "/" (parts ++ [ getNameFromPath currentPath ])

                                cmd =
                                    DocumentTemplateDraftsApi.moveFolder cfg.documentTemplateId currentPath newPath appState (cfg.wrapMsg << MoveCompleted newPath)
                            in
                            ( { model | moving = ActionResult.Loading }, cmd )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        MoveCompleted newName result ->
            case result of
                Ok _ ->
                    case model.state of
                        MovingFile file ->
                            ( { model | state = Closed }
                            , dispatch (cfg.onRenameFile file.uuid newName)
                            )

                        MovingAsset asset ->
                            ( { model | state = Closed }
                            , dispatch (cfg.onRenameAsset asset.uuid newName)
                            )

                        MovingFolder folderPath ->
                            ( { model | state = Closed }
                            , dispatch (cfg.onRenameFolder folderPath newName)
                            )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    ( { model | moving = ApiError.toActionResult appState (gettext "Move failed" appState.locale) error }
                    , Cmd.none
                    )


view : AppState -> FileTree -> Model -> Html Msg
view appState fileTree model =
    let
        modalContent =
            [ strong [ class "d-block mb-1" ] [ text (gettext "Select a new folder" appState.locale) ]
            , div [ class "move-modal-tree" ]
                [ ul [] [ viewNode appState model fileTree ]
                ]
            ]
    in
    Modal.confirm appState
        { modalTitle = gettext "Move" appState.locale
        , modalContent = modalContent
        , visible = model.state /= Closed
        , actionResult = model.moving
        , actionName = gettext "Move" appState.locale
        , actionMsg = MoveSubmit
        , cancelMsg = Just (SetState Closed)
        , dangerous = False
        , dataCy = "move-modal"
        }


viewNode : AppState -> Model -> FileTree -> Html Msg
viewNode appState model fileTree =
    case fileTree of
        FileTree.Folder folderData ->
            viewFolder appState model folderData

        _ ->
            emptyNode


viewFolder : AppState -> Model -> FileTree.FolderData -> Html Msg
viewFolder appState model folderData =
    let
        children =
            if List.isEmpty folderData.children then
                []

            else
                List.map (viewNode appState model) (List.sortBy FileTree.getName folderData.children)

        isAllowed_ =
            isAllowed model folderData.path

        isSelected =
            Just folderData.path == model.selected

        onClickHandler =
            if isAllowed_ then
                [ onClick (SetSelected folderData.path) ]

            else
                []
    in
    li []
        [ a
            (classList
                [ ( "disabled", not isAllowed_ )
                , ( "selected", isSelected )
                ]
                :: onClickHandler
            )
            [ fas "fa-folder me-2"
            , span [] [ text folderData.name ]
            ]
        , ul [] children
        ]


isAllowed : Model -> String -> Bool
isAllowed model path =
    case model.state of
        MovingFile file ->
            path /= getParentFolderPath file.fileName

        MovingAsset _ ->
            True

        MovingFolder folderPath ->
            not (String.startsWith folderPath path)

        _ ->
            False


getNameFromPath : String -> String
getNameFromPath path =
    String.split "/" path
        |> List.last
        |> Maybe.withDefault path


getParentFolderPath : String -> String
getParentFolderPath path =
    String.split "/" path
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> String.join "/"
