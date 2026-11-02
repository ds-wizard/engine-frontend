module Common.Components.Questionnaire.Modal.FileDelete exposing
    ( Model
    , Msg
    , SelectedFile
    , UpdateConfig
    , init
    , open
    , update
    , view
    )

{-| Deleting the file a file question was answered with. The file goes from the server first; only then is the answer
cleared, so a failed delete leaves the question as it was, with the reason said
in the dialog.
-}

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.ProjectFiles as ProjectFilesApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Data.AppState exposing (AppState)
import Gettext exposing (gettext)
import Html exposing (Html, p, strong, text)
import Maybe.Extra as Maybe
import String.Format as String
import Task
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Uuid
    , file : Maybe SelectedFile
    , deleting : ActionResult ()
    }


type alias SelectedFile =
    { fileUuid : Uuid
    , fileName : String
    , questionPath : String
    }


init : Uuid -> Model
init projectUuid =
    { projectUuid = projectUuid
    , file = Nothing
    , deleting = Unset
    }


type Msg
    = Open SelectedFile
    | Close
    | Delete
    | DeleteCompleted (Result ApiError ())


open : Uuid -> String -> String -> Msg
open fileUuid fileName questionPath =
    Open { fileUuid = fileUuid, fileName = fileName, questionPath = questionPath }


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , deleteFileMsg : String -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Open file ->
            ( { model | file = Just file, deleting = Unset }, Cmd.none )

        Close ->
            ( { model | file = Nothing }, Cmd.none )

        Delete ->
            case model.file of
                Just file ->
                    ( { model | deleting = Loading }
                    , ProjectFilesApi.delete appState model.projectUuid file.fileUuid (cfg.wrapMsg << DeleteCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted result ->
            case ( result, model.file ) of
                ( Ok _, Just file ) ->
                    ( { model | file = Nothing, deleting = Success () }
                    , Task.perform (always (cfg.deleteFileMsg file.questionPath)) (Task.succeed ())
                    )

                ( Ok _, Nothing ) ->
                    ( model, Cmd.none )

                ( Err error, _ ) ->
                    ( { model | deleting = ApiError.toActionResult appState (gettext "Unable to delete the file." appState.locale) error }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    ConfirmModal.dangerConfirm appState
        { title = gettext "Delete file" appState.locale
        , open = Maybe.isJust model.file
        , body =
            case model.file of
                Just file ->
                    [ p [] (String.formatHtml (gettext "Are you sure you want to delete %s?" appState.locale) [ strong [] [ text file.fileName ] ]) ]

                Nothing ->
                    []
        , confirmLabel = gettext "Delete" appState.locale
        , onConfirm = Delete
        , onClose = Close
        , result = model.deleting
        , testId = Just "file-delete"
        }
