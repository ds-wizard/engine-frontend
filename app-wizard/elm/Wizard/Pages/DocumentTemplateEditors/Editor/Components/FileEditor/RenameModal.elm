module Wizard.Pages.DocumentTemplateEditors.Editor.Components.FileEditor.RenameModal exposing
    ( Model
    , Msg
    , RenameModalState
    , UpdateConfig
    , initialModel
    , openAsset
    , openFile
    , openFolder
    , update
    , view
    )

import ActionResult exposing (ActionResult)
import Common.Components.Modal as Modal
import Common.Data.ApiError as ApiError exposing (ApiError)
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html, form, input, p, text)
import Html.Attributes exposing (class, classList, id, value)
import Html.Events exposing (onInput, onSubmit)
import List.Extra as List
import Task.Extra as Task
import Uuid exposing (Uuid)
import Wizard.Api.DocumentTemplateDrafts as DocumentTemplateDraftsApi
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateAsset exposing (DocumentTemplateAsset)
import Wizard.Api.Models.DocumentTemplate.DocumentTemplateFile exposing (DocumentTemplateFile)
import Wizard.Data.AppState exposing (AppState)


type alias Model =
    { state : RenameModalState
    , input : String
    , renaming : ActionResult String
    }


type RenameModalState
    = Closed
    | RenamingFile DocumentTemplateFile
    | RenamingAsset DocumentTemplateAsset
    | RenamingFolder String


initialModel : Model
initialModel =
    { state = Closed
    , input = ""
    , renaming = ActionResult.Unset
    }


type Msg
    = SetState RenameModalState
    | RenameInput String
    | RenameSubmit
    | RenameCompleted String (Result ApiError ())


openFile : DocumentTemplateFile -> Msg
openFile file =
    SetState (RenamingFile file)


openAsset : DocumentTemplateAsset -> Msg
openAsset asset =
    SetState (RenamingAsset asset)


openFolder : String -> Msg
openFolder path =
    SetState (RenamingFolder path)


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , documentTemplateId : String
    , selectedFolderPath : String
    , fileContents : Dict String (ActionResult String)
    , onRenameFile : Uuid -> String -> msg
    , onRenameAsset : Uuid -> String -> msg
    , onRenameFolder : String -> String -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        SetState state ->
            let
                renameFileModalInput =
                    case state of
                        RenamingFile file ->
                            getNameFromPath file.fileName

                        RenamingAsset asset ->
                            getNameFromPath asset.fileName

                        RenamingFolder path ->
                            getNameFromPath path

                        _ ->
                            ""
            in
            ( { model
                | state = state
                , input = renameFileModalInput
                , renaming = ActionResult.Unset
              }
            , Cmd.none
            )

        RenameInput input ->
            ( { model | input = input }, Cmd.none )

        RenameSubmit ->
            if not (String.isEmpty model.input) then
                case model.state of
                    RenamingFile file ->
                        let
                            fileName =
                                String.join "/" (List.filter (not << String.isEmpty) [ cfg.selectedFolderPath, model.input ])

                            fileContent =
                                Dict.get (Uuid.toString file.uuid) cfg.fileContents
                                    |> Maybe.withDefault (ActionResult.Success "")
                                    |> ActionResult.withDefault ""

                            templateFile =
                                { file | fileName = fileName }

                            cmd =
                                DocumentTemplateDraftsApi.putFile appState cfg.documentTemplateId templateFile fileContent (cfg.wrapMsg << RenameCompleted fileName)
                        in
                        ( { model | renaming = ActionResult.Loading }
                        , cmd
                        )

                    RenamingAsset asset ->
                        let
                            assetName =
                                String.join "/" (List.filter (not << String.isEmpty) [ cfg.selectedFolderPath, model.input ])

                            templateAsset =
                                { asset | fileName = assetName }

                            cmd =
                                DocumentTemplateDraftsApi.putAsset appState cfg.documentTemplateId templateAsset (cfg.wrapMsg << RenameCompleted assetName)
                        in
                        ( { model | renaming = ActionResult.Loading }
                        , cmd
                        )

                    RenamingFolder path ->
                        let
                            currentName =
                                path

                            parts =
                                String.split "/" path

                            newPath =
                                List.removeAt (List.length parts - 1) parts

                            newName =
                                String.join "/" (newPath ++ [ model.input ])

                            cmd =
                                DocumentTemplateDraftsApi.moveFolder appState cfg.documentTemplateId currentName newName (cfg.wrapMsg << RenameCompleted newName)
                        in
                        ( { model | renaming = ActionResult.Loading }
                        , cmd
                        )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        RenameCompleted newName result ->
            case result of
                Ok _ ->
                    case model.state of
                        RenamingFile file ->
                            ( { model | state = Closed }
                            , Task.dispatch (cfg.onRenameFile file.uuid newName)
                            )

                        RenamingAsset asset ->
                            ( { model | state = Closed }
                            , Task.dispatch (cfg.onRenameAsset asset.uuid newName)
                            )

                        RenamingFolder currentName ->
                            ( { model | state = Closed }
                            , Task.dispatch (cfg.onRenameFolder currentName newName)
                            )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    ( { model | renaming = ApiError.toActionResult appState (gettext "Rename failed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )


getNameFromPath : String -> String
getNameFromPath path =
    String.split "/" path
        |> List.last
        |> Maybe.withDefault path


view : AppState -> Model -> Html Msg
view appState model =
    let
        modalContent =
            [ form [ onSubmit RenameSubmit ]
                [ input
                    [ class "form-control"
                    , classList [ ( "is-invalid", String.isEmpty model.input ) ]
                    , id "new-file-name"
                    , value model.input
                    , onInput RenameInput
                    ]
                    []
                , p [ class "invalid-feedback" ] [ text (gettext "Name cannot be empty." appState.locale) ]
                ]
            ]

        cfg =
            Modal.confirmConfig (gettext "Rename" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible (model.state /= Closed)
                |> Modal.confirmConfigActionResult model.renaming
                |> Modal.confirmConfigAction (gettext "Rename" appState.locale) RenameSubmit
                |> Modal.confirmConfigCancelMsg (SetState Closed)
                |> Modal.confirmConfigDataCy "add-file-modal"
    in
    Modal.confirm appState cfg
