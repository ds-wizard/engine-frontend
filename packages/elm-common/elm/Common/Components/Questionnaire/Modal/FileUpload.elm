module Common.Components.Questionnaire.Modal.FileUpload exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , open
    , update
    , view
    )

{-| Uploading the file a file question asks for.

The file is chosen by button or by dropping it (`Ui.FileUpload`), checked against
what the question accepts — its file types, by media type or extension, and the
smaller of its own size limit and the server's — and posted. What is wrong with a
chosen file is the field's error, said under it and read with its remove button,
so the dialog's Upload does nothing until the file is one the question takes. In
the knowledge model editor's preview there is nowhere to upload to, and the
dialog says so instead.

-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.ProjectFileSimple exposing (ProjectFileSimple)
import Common.Api.ProjectFiles as ProjectFilesApi
import Common.Components.ApiErrorAlert as ApiErrorAlert
import Common.Components.Questionnaire.ContentMsg exposing (FileConfig)
import Common.Components.Questionnaire.Path as Path
import Common.Components.UiWords as UiWords
import Common.Data.AppState exposing (AppState)
import Common.Utils.ByteUnits as ByteUnits
import Common.Utils.FileUtils as FileUtils
import File exposing (File)
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Ui.Alert as Alert
import Ui.FileUpload as FileUpload
import Ui.Modal as Modal
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , open : Bool
    , questionPath : String
    , fileConfig : FileConfig
    , kmEditor : Bool
    , upload : FileUpload.Model
    , submitting : ActionResult ()
    }


init : Uuid -> Model
init projectUuid =
    { projectUuid = projectUuid
    , open = False
    , questionPath = ""
    , fileConfig = { fileTypes = Nothing, maxSize = Nothing }
    , kmEditor = False
    , upload = FileUpload.init
    , submitting = Unset
    }


type Msg
    = Open String FileConfig
    | Close
    | UploadMsg FileUpload.Msg
    | ClearFile
    | Save
    | SaveCompleted (Result ApiError ProjectFileSimple)


open : String -> FileConfig -> Msg
open =
    Open


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , kmEditor : Bool
    , setFileMsg : String -> ProjectFileSimple -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Open questionPath fileConfig ->
            ( { model
                | open = True
                , questionPath = questionPath
                , fileConfig = fileConfig
                , kmEditor = cfg.kmEditor
                , upload = FileUpload.clear
                , submitting = Unset
              }
            , Cmd.none
            )

        Close ->
            ( { model | open = False }, Cmd.none )

        UploadMsg uploadMsg ->
            let
                ( upload, cmd ) =
                    FileUpload.update { mimes = fileTypes model.fileConfig, read = False } uploadMsg model.upload
            in
            ( { model | upload = upload }, Cmd.map (cfg.wrapMsg << UploadMsg) cmd )

        ClearFile ->
            ( { model | upload = FileUpload.clear }, Cmd.none )

        Save ->
            case ( FileUpload.file model.upload, model.kmEditor ) of
                ( Just file, False ) ->
                    if Maybe.isNothing (problem appState model file) then
                        ( { model | submitting = Loading }
                        , ProjectFilesApi.post appState model.projectUuid (questionUuid model.questionPath) file (cfg.wrapMsg << SaveCompleted)
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SaveCompleted result ->
            case result of
                Ok file ->
                    ( { model | submitting = Success (), open = False }
                    , Task.perform (always (cfg.setFileMsg model.questionPath file)) (Task.succeed ())
                    )

                Err error ->
                    ( { model | submitting = ApiError.toActionResult appState (gettext "Unable to upload the file." appState.locale) error }
                    , Cmd.none
                    )


questionUuid : String -> String
questionUuid questionPath =
    String.split "." questionPath
        |> List.last
        |> Maybe.withDefault ""


fileTypes : FileConfig -> List String
fileTypes fileConfig =
    case fileConfig.fileTypes of
        Just types ->
            List.map String.trim (String.split "," types)

        Nothing ->
            []


maxSize : AppState -> FileConfig -> Int
maxSize appState fileConfig =
    case fileConfig.maxSize of
        Just questionMax ->
            min appState.maxUploadFileSize questionMax

        Nothing ->
            appState.maxUploadFileSize


{-| What keeps a chosen file from being uploaded: a type the question does not
take, or a size over the limit.
-}
problem : AppState -> Model -> File -> Maybe String
problem appState model file =
    let
        types =
            fileTypes model.fileConfig

        extension =
            "." ++ FileUtils.getExtension (File.name file)
    in
    if not (List.isEmpty types) && List.notMember (File.mime file) types && List.notMember extension types then
        Just (String.format (gettext "The file type is not allowed for this question. Choose one of the following: %s" appState.locale) [ String.join ", " types ])

    else if File.size file > maxSize appState model.fileConfig then
        Just (String.format (gettext "The file cannot be larger than %s." appState.locale) [ ByteUnits.toReadable (maxSize appState model.fileConfig) ])

    else
        Nothing


view : AppState -> Model -> Html Msg
view appState model =
    let
        body =
            if model.kmEditor then
                [ Alert.toHtml (Alert.info (gettext "File upload is not available in the knowledge model editor" appState.locale)) ]

            else
                let
                    hint =
                        case fileTypes model.fileConfig of
                            [] ->
                                String.format (gettext "Up to %s." appState.locale) [ ByteUnits.toReadable (maxSize appState model.fileConfig) ]

                            types ->
                                String.format (gettext "%s, up to %s." appState.locale) [ String.join ", " types, ByteUnits.toReadable (maxSize appState model.fileConfig) ]
                in
                [ ApiErrorAlert.view appState model.submitting
                , FileUpload.toHtmlWithError (UiWords.fileUpload appState)
                    { id = Path.toFieldId model.questionPath ++ "-upload"
                    , toMsg = UploadMsg
                    , label = gettext "File" appState.locale
                    , buttonLabel = gettext "Choose file" appState.locale
                    , hint = hint
                    , error = Maybe.andThen (problem appState model) (FileUpload.file model.upload)
                    , onClear = Just ClearFile
                    }
                    model.upload
                ]
    in
    Modal.plain
        { title = gettext "Upload a file" appState.locale
        , open = model.open
        , onClose = Close
        , body = body
        }
        |> Modal.withForm { onSubmit = Save, submitLabel = gettext "Upload" appState.locale }
        |> Modal.withLoading (ActionResult.isLoading model.submitting)
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withTestId "file-upload"
        |> Modal.toHtml
