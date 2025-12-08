module Wizard.Components.Questionnaire.FileUploadModal exposing
    ( FileConfig
    , Model
    , Msg
    , UpdateConfig
    , init
    , open
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.ActionButton as ActionButton
import Common.Components.FontAwesome exposing (fa, faCancel)
import Common.Components.FormResult as FormResult
import Common.Components.Modal as Modal
import Common.Components.Tooltip exposing (tooltip)
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.FileIcon as FileIcon
import Common.Utils.FileUtils as FileUtils
import Common.Utils.Markdown as Markdown
import File exposing (File)
import File.Select as Select
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, p, span, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Events.Extensions exposing (alwaysPreventDefaultOn, alwaysPreventDefaultOnWithDecoder)
import Html.Extra as Html
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.Models.ProjectFileSimple exposing (ProjectFileSimple)
import Wizard.Api.ProjectFiles as ProjectFilesApi
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { projectUuid : Uuid
    , isOpen : Bool
    , questionPath : String
    , fileConfig : FileConfig
    , hover : Bool
    , file : Maybe File
    , submitting : ActionResult ()
    }


type alias FileConfig =
    { fileTypes : Maybe String
    , maxSize : Maybe Int
    }


init : Uuid -> Model
init uuid =
    { projectUuid = uuid
    , questionPath = ""
    , fileConfig =
        { fileTypes = Nothing
        , maxSize = Nothing
        }
    , isOpen = False
    , hover = False
    , file = Nothing
    , submitting = ActionResult.Unset
    }


type Msg
    = Open String FileConfig
    | Close
    | Pick
    | DragEnter
    | DragLeave
    | GotFile File
    | ClearFile
    | Save
    | SaveCompleted (Result ApiError ProjectFileSimple)


open : String -> FileConfig -> Msg
open =
    Open


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , setFileMsg : String -> ProjectFileSimple -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Open questionPath fileConfig ->
            ( { model
                | isOpen = True
                , questionPath = questionPath
                , file = Nothing
                , fileConfig = fileConfig
                , submitting = ActionResult.Unset
              }
            , Cmd.none
            )

        Close ->
            ( { model | isOpen = False }, Cmd.none )

        Pick ->
            let
                fileTypes =
                    case model.fileConfig.fileTypes of
                        Just types ->
                            String.split "," types
                                |> List.map String.trim

                        Nothing ->
                            []
            in
            ( model, Select.file fileTypes (cfg.wrapMsg << GotFile) )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFile file ->
            ( { model | hover = False, file = Just file }
            , Cmd.none
            )

        ClearFile ->
            ( { model | file = Nothing }
            , Cmd.none
            )

        Save ->
            case model.file of
                Just file ->
                    let
                        questionUuidString =
                            String.split "." model.questionPath
                                |> List.last
                                |> Maybe.withDefault ""
                    in
                    ( { model | submitting = ActionResult.Loading }
                    , ProjectFilesApi.post appState model.projectUuid questionUuidString file (cfg.wrapMsg << SaveCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        SaveCompleted result ->
            case result of
                Ok file ->
                    ( { model
                        | submitting = ActionResult.Success ()
                        , isOpen = False
                      }
                    , Task.dispatch (cfg.setFileMsg model.questionPath file)
                    )

                Err error ->
                    ( { model | submitting = ApiError.toActionResult appState (gettext "Unable to upload the file." appState.locale) error }
                    , Cmd.none
                    )


view : AppState -> Bool -> Model -> Html Msg
view appState isKmEditor model =
    let
        invalidFileSelection =
            case model.file of
                Just file ->
                    not (isValidFileType model.fileConfig file) || not (isValidFileSize appState model.fileConfig file)

                Nothing ->
                    True

        submitButtonDisabled =
            if isKmEditor then
                True

            else
                invalidFileSelection

        submitButtonTooltip =
            if isKmEditor && not invalidFileSelection then
                tooltip (gettext "File upload is not available in the knowledge model editor" appState.locale)

            else
                []

        submitButton =
            span submitButtonTooltip
                [ ActionButton.buttonWithAttrs
                    { label = gettext "Upload" appState.locale
                    , result = model.submitting
                    , msg = Save
                    , dangerous = False
                    , attrs =
                        [ disabled submitButtonDisabled
                        , dataCy "modal_action-button"
                        ]
                    }
                ]

        cancelButton =
            button
                [ class "btn btn-secondary"
                , onClick Close
                , disabled (ActionResult.isLoading model.submitting)
                , dataCy "modal_cancel-button"
                ]
                [ text (gettext "Cancel" appState.locale) ]

        content =
            case model.file of
                Just file ->
                    contentFileView appState model file

                Nothing ->
                    contentDropzoneView appState model

        modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Upload a file" appState.locale) ] ]
            , div [ class "modal-body logo-upload" ]
                [ FormResult.errorOnlyView model.submitting
                , content
                ]
            , div [ class "modal-footer" ]
                [ submitButton
                , cancelButton
                ]
            ]

        ( enterMsg, escMsg ) =
            if ActionResult.isLoading model.submitting then
                ( Nothing, Nothing )

            else
                ( Just Save, Just Close )

        modalConfig =
            { modalContent = modalContent
            , visible = model.isOpen
            , enterMsg = enterMsg
            , escMsg = escMsg
            , dataCy = "file-upload"
            }
    in
    Modal.simple modalConfig


contentFileView : AppState -> Model -> File -> Html Msg
contentFileView appState model file =
    let
        fileTypeError =
            if isValidFileType model.fileConfig file then
                Html.nothing

            else
                let
                    fileTypes =
                        Maybe.withDefault "" model.fileConfig.fileTypes
                            |> String.split ","
                            |> List.map (\t -> "\n- " ++ String.trim t)
                            |> String.concat
                in
                div [ class "alert alert-danger" ]
                    [ Markdown.toHtml []
                        (String.format
                            (gettext "**The file type is not allowed for this question.**\n\nChoose one of the following: %s" appState.locale)
                            [ fileTypes ]
                        )
                    ]

        fileSizeError =
            if isValidFileSize appState model.fileConfig file then
                Html.nothing

            else
                div [ class "alert alert-danger" ]
                    [ Markdown.toHtml []
                        (String.format
                            (gettext "The file cannot be larger than %s." appState.locale)
                            [ ByteUnits.toReadable (getFileMaxSize appState model.fileConfig) ]
                        )
                    ]
    in
    div []
        [ fileTypeError
        , fileSizeError
        , div [ class "rounded-3 bg-light mb-1 px-3 py-3 d-flex justify-content-between align-items-center" ]
            [ div [ class "d-flex overflow-hidden" ]
                [ span [ class "me-2" ] [ fa (FileIcon.getFileIcon (File.name file) (File.mime file)) ]
                , span [ class "flex-grow-1 text-truncate" ] [ text (File.name file) ]
                , span [ class "text-muted ms-2 text-nowrap" ]
                    [ text ("(" ++ (ByteUnits.toReadable (File.size file) ++ ")")) ]
                ]
            , Html.viewIf (not (ActionResult.isLoading model.submitting)) <|
                a
                    [ class "text-danger ms-2"
                    , onClick ClearFile
                    , disabled (ActionResult.isLoading model.submitting)
                    ]
                    [ faCancel ]
            ]
        ]


contentDropzoneView : AppState -> Model -> Html Msg
contentDropzoneView appState model =
    div
        [ class "dropzone"
        , classList [ ( "active", model.hover ) ]
        , alwaysPreventDefaultOn "dragenter" DragEnter
        , alwaysPreventDefaultOn "dragover" DragEnter
        , alwaysPreventDefaultOn "dragleave" DragLeave
        , alwaysPreventDefaultOnWithDecoder "drop" dropDecoder
        ]
        [ button [ onClick Pick, class "btn btn-secondary" ] [ text (gettext "Choose file" appState.locale) ]
        , p [] [ text (gettext "Or just drop it here" appState.locale) ]
        ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files", "0" ] (D.map GotFile File.decoder)


isValidFileType : FileConfig -> File -> Bool
isValidFileType fileConfig file =
    case fileConfig.fileTypes of
        Just types ->
            let
                fileTypes =
                    String.split "," types
                        |> List.map String.trim

                fileMime =
                    File.mime file

                fileExtension =
                    File.name file
                        |> FileUtils.getExtension
                        |> (++) "."
            in
            List.member fileMime fileTypes || List.member fileExtension fileTypes

        Nothing ->
            True


isValidFileSize : AppState -> FileConfig -> File -> Bool
isValidFileSize appState fileConfig file =
    File.size file <= getFileMaxSize appState fileConfig


getFileMaxSize : AppState -> FileConfig -> Int
getFileMaxSize appState fileConfig =
    Maybe.unwrap appState.maxUploadFileSize (min appState.maxUploadFileSize) fileConfig.maxSize
