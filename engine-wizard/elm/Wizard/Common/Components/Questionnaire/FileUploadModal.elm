module Wizard.Common.Components.Questionnaire.FileUploadModal exposing
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
import File exposing (File)
import File.Select as Select
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, h5, p, span, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Json.Decode as D
import Shared.Api.QuestionnaireFiles as QuestionnaireFilesApi
import Shared.Common.ByteUnits as ByteUnits
import Shared.Data.QuestionnaireFileSimple exposing (QuestionnaireFileSimple)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (fa, faSet)
import Shared.Markdown as Markdown
import Shared.Utils exposing (dispatch)
import String.Format as String
import Uuid exposing (Uuid)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.FileIcon as FileIcon
import Wizard.Common.FileUtils as FileUtils
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.Html.Events exposing (alwaysPreventDefaultOn)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal


type alias Model =
    { questionnaireUuid : Uuid
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
    { questionnaireUuid = uuid
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
    | SaveCompleted (Result ApiError QuestionnaireFileSimple)


open : String -> FileConfig -> Msg
open =
    Open


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , setFileMsg : String -> QuestionnaireFileSimple -> msg
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
                    ( { model | submitting = ActionResult.Loading }
                    , QuestionnaireFilesApi.postFile model.questionnaireUuid file appState (cfg.wrapMsg << SaveCompleted)
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
                    , dispatch (cfg.setFileMsg model.questionPath file)
                    )

                Err error ->
                    ( { model | submitting = ApiError.toActionResult appState (gettext "Unable to upload the file." appState.locale) error }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    let
        submitButtonDisabled =
            case model.file of
                Just file ->
                    not (isValidFileType model.fileConfig file) || not (isValidFileSize model.fileConfig file)

                Nothing ->
                    True

        submitButton =
            ActionButton.buttonWithAttrs appState
                { label = gettext "Save" appState.locale
                , result = model.submitting
                , msg = Save
                , dangerous = False
                , attrs =
                    [ disabled submitButtonDisabled
                    , dataCy "modal_action-button"
                    ]
                }

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
                [ FormResult.errorOnlyView appState model.submitting
                , content
                ]
            , div [ class "modal-footer" ]
                [ submitButton
                , cancelButton
                ]
            ]

        modalConfig =
            { modalContent = modalContent
            , visible = model.isOpen
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
            if isValidFileSize model.fileConfig file then
                Html.nothing

            else
                div [ class "alert alert-danger" ]
                    [ Markdown.toHtml []
                        (String.format
                            (gettext "The file cannot be larger than %s." appState.locale)
                            [ ByteUnits.toReadable (Maybe.withDefault 0 model.fileConfig.maxSize) ]
                        )
                    ]
    in
    div []
        [ fileTypeError
        , fileSizeError
        , div [ class "rounded-3 bg-light mb-1 px-3 py-3 d-flex justify-content-between align-items-center" ]
            [ div []
                [ span [ class "me-2" ] [ fa (FileIcon.getFileIcon (File.name file) (File.mime file)) ]
                , text (File.name file)
                , span [ class "text-muted ms-2" ]
                    [ text ("(" ++ (ByteUnits.toReadable (File.size file) ++ ")")) ]
                ]
            , Html.viewIf (not (ActionResult.isLoading model.submitting)) <|
                a
                    [ class "text-danger"
                    , onClick ClearFile
                    , disabled (ActionResult.isLoading model.submitting)
                    ]
                    [ faSet "_global.cancel" appState ]
            ]
        ]


contentDropzoneView : AppState -> Model -> Html Msg
contentDropzoneView appState model =
    div
        [ class "dropzone"
        , classList [ ( "active", model.hover ) ]
        , alwaysPreventDefaultOn "dragenter" (D.succeed DragEnter)
        , alwaysPreventDefaultOn "dragover" (D.succeed DragEnter)
        , alwaysPreventDefaultOn "dragleave" (D.succeed DragLeave)
        , alwaysPreventDefaultOn "drop" dropDecoder
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


isValidFileSize : FileConfig -> File -> Bool
isValidFileSize fileConfig file =
    case fileConfig.maxSize of
        Just maxSize ->
            File.size file <= maxSize

        Nothing ->
            True