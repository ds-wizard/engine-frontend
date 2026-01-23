module Wizard.Components.Questionnaire2.Components.FileDeleteModal exposing (Model, Msg, SelectedFile, UpdateConfig, init, open, update, view)

import ActionResult exposing (ActionResult)
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Components.Modal as Modal
import Gettext exposing (gettext)
import Html exposing (Html, strong, text)
import Html.Attributes exposing (class)
import Html.Lazy as Lazy
import Maybe.Extra as Maybe
import String.Format as String
import Task.Extra as Taks
import Uuid exposing (Uuid)
import Wizard.Api.ProjectFiles as ProjectFilesApi
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { projectUuid : Uuid
    , file : Maybe SelectedFile
    , deletingFile : ActionResult ()
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
    , deletingFile = ActionResult.Unset
    }


type Msg
    = Open SelectedFile
    | Close
    | Delete
    | DeleteCompleted (Result ApiError ())


open : Uuid -> String -> String -> Msg
open fileUuid fileName questionPath =
    Open (SelectedFile fileUuid fileName questionPath)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , deleteFileMsg : String -> msg
    }


update : AppState -> UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update appState cfg msg model =
    case msg of
        Open selectedFile ->
            ( { model
                | file = Just selectedFile
                , deletingFile = ActionResult.Unset
              }
            , Cmd.none
            )

        Close ->
            ( { model | file = Nothing }, Cmd.none )

        Delete ->
            case model.file of
                Just selectedFile ->
                    ( { model | deletingFile = ActionResult.Loading }
                    , ProjectFilesApi.delete appState model.projectUuid selectedFile.fileUuid (cfg.wrapMsg << DeleteCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCompleted result ->
            case result of
                Ok _ ->
                    case model.file of
                        Just selectedFile ->
                            ( { model
                                | file = Nothing
                                , deletingFile = ActionResult.Success ()
                              }
                            , Taks.dispatch (cfg.deleteFileMsg selectedFile.questionPath)
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Err error ->
                    ( { model | deletingFile = ApiError.toActionResult appState (gettext "Unable to delete the file." appState.locale) error }
                    , Cmd.none
                    )


view : AppState -> Model -> Html Msg
view appState model =
    Lazy.lazy2 viewLazy appState.locale model


viewLazy : Gettext.Locale -> Model -> Html Msg
viewLazy locale model =
    let
        modalContent =
            case model.file of
                Just file ->
                    String.formatHtml (gettext "Are you sure you want to delete %s?" locale)
                        [ strong [ class "text-break" ] [ text file.fileName ] ]

                Nothing ->
                    []

        cfg =
            Modal.confirmConfig (gettext "Delete file" locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (Maybe.isJust model.file)
                |> Modal.confirmConfigAction (gettext "Delete" locale) Delete
                |> Modal.confirmConfigCancelMsg Close
                |> Modal.confirmConfigDangerous True
                |> Modal.confirmConfigDataCy "delete-file"
    in
    Modal.confirm { locale = locale } cfg
