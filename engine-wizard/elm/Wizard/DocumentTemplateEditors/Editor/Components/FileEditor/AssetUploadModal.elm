module Wizard.DocumentTemplateEditors.Editor.Components.FileEditor.AssetUploadModal exposing
    ( Model
    , Msg(..)
    , UpdateConfig
    , initialModel
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Dict exposing (Dict)
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
import Shared.Html exposing (faSetFw)
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
    , files : Maybe (List File)
    , fileContents : Dict String String
    , open : Bool
    , submitting : Dict String (ActionResult ())
    , count : Int
    }


initialModel : Model
initialModel =
    { hover = False
    , files = Nothing
    , fileContents = Dict.empty
    , open = False
    , submitting = Dict.empty
    , count = 0
    }



-- UPDATE


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | PickedFiles File (List File)
    | GotFiles (List File)
    | GotFileContent String String
    | SetOpen Bool
    | Upload
    | SubmitAssetComplete String (Result ApiError DocumentTemplateAsset)
    | SubmitFileComplete String (Result ApiError DocumentTemplateFile)


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
            ( model, Cmd.map cfg.wrapMsg <| Select.files [ "*/*" ] PickedFiles )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        PickedFiles file files ->
            ( { model | hover = False, files = Just (file :: files) }
            , Cmd.none
            )

        GotFiles files ->
            ( { model | hover = False, files = Just files }
            , Cmd.none
            )

        GotFileContent file fileContent ->
            let
                fileContents =
                    Dict.insert file fileContent model.fileContents
            in
            case ( Dict.size fileContents >= model.count, model.files ) of
                ( True, Just files ) ->
                    uploadFilesAndAssets cfg appState { model | fileContents = fileContents } files

                _ ->
                    ( model, Cmd.none )

        SetOpen open ->
            ( { model | hover = False, open = open, files = Nothing, submitting = Dict.empty }
            , Cmd.none
            )

        Upload ->
            case model.files of
                Just files ->
                    let
                        textFiles =
                            List.filter (ContentType.isText << File.mime) files

                        getContentCmd file =
                            Task.perform (cfg.wrapMsg << GotFileContent (File.name file)) (File.toString file)

                        textFilesCount =
                            List.length textFiles
                    in
                    if textFilesCount > 0 then
                        ( { model | count = textFilesCount }, Cmd.batch (List.map getContentCmd textFiles) )

                    else
                        uploadFilesAndAssets cfg appState model files

                _ ->
                    ( model, Cmd.none )

        SubmitAssetComplete fileName result ->
            handleSubmitComplete cfg.addAssetMsg appState model fileName result

        SubmitFileComplete fileName result ->
            handleSubmitComplete cfg.addFileMsg appState model fileName result


handleSubmitComplete : (a -> msg) -> AppState -> Model -> String -> Result ApiError a -> ( Model, Cmd msg )
handleSubmitComplete dispatchMsg appState model fileName result =
    case result of
        Ok documentTemplateFile ->
            let
                submitting =
                    Dict.insert fileName (Success ()) model.submitting

                allSubmitted =
                    List.all ActionResult.isSuccess (Dict.values submitting)
            in
            ( { model | open = not allSubmitted, submitting = submitting }
            , dispatch (dispatchMsg documentTemplateFile)
            )

        Err error ->
            ( { model
                | submitting =
                    Dict.insert
                        fileName
                        (ApiError.toActionResult appState (gettext "Unable to upload file." appState.locale) error)
                        model.submitting
              }
            , Cmd.none
            )


uploadFilesAndAssets : UpdateConfig msg -> AppState -> Model -> List File -> ( Model, Cmd msg )
uploadFilesAndAssets cfg appState model files =
    let
        upload file ( actionResults, cmds ) =
            let
                fileName =
                    File.name file

                cmd =
                    case Dict.get (File.name file) model.fileContents of
                        Just fileContent ->
                            uploadFile cfg appState file fileContent

                        Nothing ->
                            uploadAsset cfg appState file
            in
            ( Dict.insert fileName Loading actionResults
            , cmd :: cmds
            )

        ( submitting, uploadCmds ) =
            List.foldl upload ( Dict.empty, [] ) files
    in
    ( { model | submitting = submitting }, Cmd.batch uploadCmds )


uploadAsset : UpdateConfig msg -> AppState -> File -> Cmd msg
uploadAsset cfg appState file =
    DocumentTemplateDraftsApi.uploadAsset cfg.documentTemplateId
        (getFileName cfg.path file)
        file
        appState
        (cfg.wrapMsg << SubmitAssetComplete (File.name file))


uploadFile : UpdateConfig msg -> AppState -> File -> String -> Cmd msg
uploadFile cfg appState file content =
    let
        documentTemplateFile =
            { uuid = Uuid.nil
            , fileName = getFileName cfg.path file
            }
    in
    DocumentTemplateDraftsApi.postFile cfg.documentTemplateId
        documentTemplateFile
        content
        appState
        (cfg.wrapMsg << SubmitFileComplete (File.name file))


getFileName : String -> File -> String
getFileName path file =
    String.join "/" (List.filter (not << String.isEmpty) [ path, File.name file ])



-- VIEW


view : AppState -> Model -> Html Msg
view appState model =
    let
        actionResult =
            ActionResult.all (Dict.values model.submitting)

        submitButtonDisabled =
            Maybe.isNothing model.files

        submitButton =
            ActionButton.buttonWithAttrs appState
                { label = gettext "Upload" appState.locale
                , result = actionResult
                , msg = Upload
                , dangerous = False
                , attrs = [ disabled submitButtonDisabled, dataCy "modal_action-button" ]
                }

        cancelButton =
            button [ class "btn btn-secondary", onClick (SetOpen False), disabled (ActionResult.isLoading actionResult) ]
                [ text (gettext "Cancel" appState.locale) ]

        fileContent =
            case model.files of
                Just files ->
                    filesView appState files

                Nothing ->
                    dropzone appState model

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Upload files" appState.locale) ] ]
            , div [ class "modal-body logo-upload" ]
                [ FormResult.errorOnlyView appState actionResult
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
        [ button [ onClick Pick, class "btn btn-secondary" ] [ text (gettext "Choose files" appState.locale) ]
        , p [] [ text (gettext "Or just drop them here" appState.locale) ]
        ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.map GotFiles (D.list File.decoder))


filesView : AppState -> List File -> Html Msg
filesView appState files =
    let
        fileView file =
            div [ class "rounded-3 bg-light mb-1 px-3 py-2" ]
                [ faSetFw "import.file" appState
                , text (File.name file)
                ]
    in
    div [ class "rounded-3" ]
        [ div [] (List.map fileView files)
        ]
