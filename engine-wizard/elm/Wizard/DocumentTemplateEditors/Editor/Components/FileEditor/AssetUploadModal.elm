module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.AssetUploadModal exposing
    ( Model
    , Msg(..)
    , UpdateConfig
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import File exposing (File)
import File.Select as Select
import Gettext exposing (gettext)
import Html exposing (Html, button, div, h5, p, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Json.Decode as D
import Maybe.Extra as Maybe
import Shared.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Shared.Data.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Shared.Data.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (faSet)
import Shared.Utils exposing (dispatch)
import Task
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.ContentType as ContentType
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.Html.Events exposing (alwaysPreventDefaultOn)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Common.View.Modal as Modal



-- MODEL


type alias Model =
    { hover : Bool
    , file : Maybe File
    , open : Bool
    , submitting : ActionResult ()
    }


initialModel : Model
initialModel =
    { hover = False
    , file = Nothing
    , open = False
    , submitting = Unset
    }



-- UPDATE


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFile File
    | GotFileContent String
    | SetOpen Bool
    | Upload
    | SubmitAssetComplete (Result ApiError DocumentTemplateAsset)
    | SubmitFileComplete (Result ApiError DocumentTemplateFile)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , addAssetMsg : DocumentTemplateAsset -> msg
    , addFileMsg : DocumentTemplateFile -> msg
    , documentTemplateId : String
    , path : String
    }


update : UpdateConfig msg -> Msg -> AppState -> Model -> ( Model, Cmd msg )
update cfg msg appState model =
    case msg of
        Pick ->
            ( model, Cmd.map cfg.wrapMsg <| Select.file [ "*/*" ] GotFile )

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

        GotFileContent fileContent ->
            case model.file of
                Just file ->
                    uploadFile cfg appState model file fileContent

                _ ->
                    ( model, Cmd.none )

        SetOpen open ->
            ( { model | hover = False, open = open, file = Nothing, submitting = Unset }
            , Cmd.none
            )

        Upload ->
            case model.file of
                Just file ->
                    if ContentType.isText (File.mime file) then
                        ( { model | submitting = Loading }
                        , Task.perform (cfg.wrapMsg << GotFileContent) (File.toString file)
                        )

                    else
                        uploadAsset cfg appState model file

                _ ->
                    ( model, Cmd.none )

        SubmitAssetComplete result ->
            handleSubmitComplete cfg.addAssetMsg appState model result

        SubmitFileComplete result ->
            handleSubmitComplete cfg.addFileMsg appState model result


handleSubmitComplete : (a -> msg) -> AppState -> Model -> Result ApiError a -> ( Model, Cmd msg )
handleSubmitComplete dispatchMsg appState model result =
    case result of
        Ok documentTemplateFile ->
            ( { model | open = False }
            , dispatch (dispatchMsg documentTemplateFile)
            )

        Err error ->
            ( { model | submitting = ApiError.toActionResult appState (gettext "Unable to upload file." appState.locale) error }, Cmd.none )


uploadAsset : UpdateConfig msg -> AppState -> Model -> File -> ( Model, Cmd msg )
uploadAsset cfg appState model file =
    let
        cmd =
            DocumentTemplateDraftsApi.uploadAsset cfg.documentTemplateId
                (getFileName cfg.path file)
                file
                appState
                (cfg.wrapMsg << SubmitAssetComplete)
    in
    ( { model | submitting = Loading }, cmd )


uploadFile : UpdateConfig msg -> AppState -> Model -> File -> String -> ( Model, Cmd msg )
uploadFile cfg appState model file content =
    let
        documentTemplateFile =
            { uuid = Uuid.nil
            , fileName = getFileName cfg.path file
            }

        cmd =
            DocumentTemplateDraftsApi.postFile cfg.documentTemplateId
                documentTemplateFile
                content
                appState
                (cfg.wrapMsg << SubmitFileComplete)
    in
    ( { model | submitting = Loading }, cmd )


getFileName : String -> File -> String
getFileName path file =
    String.join "/" (List.filter (not << String.isEmpty) [ path, File.name file ])



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        submitButtonDisabled =
            Maybe.isNothing model.file

        submitButton =
            ActionButton.buttonWithAttrs appState
                { label = gettext "Upload" appState.locale
                , result = model.submitting
                , msg = Upload
                , dangerous = False
                , attrs = [ disabled submitButtonDisabled, dataCy "modal_action-button" ]
                }

        cancelButton =
            button [ class "btn btn-secondary", onClick (SetOpen False), disabled (ActionResult.isLoading model.submitting) ]
                [ text (gettext "Cancel" appState.locale) ]

        fileContent =
            case model.file of
                Just file ->
                    fileView appState file

                Nothing ->
                    dropzone appState model

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Upload file" appState.locale) ] ]
            , div [ class "modal-body logo-upload" ]
                [ FormResult.errorOnlyView appState model.submitting
                , fileContent
                ]
            , div [ class "modal-footer" ]
                [ submitButton
                , cancelButton
                ]
            ]

        modalConfig =
            { modalContent = content
            , visible = model.open
            , dataCy = "logo-upload"
            }
    in
    Modal.simple modalConfig


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
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


fileView : AppState -> File -> Html Msg
fileView appState file =
    div [ class "rounded-3 file-view" ]
        [ div [ class "file" ]
            [ faSet "import.file" appState
            , div [ class "filename" ]
                [ text (File.name file) ]
            ]
        ]
