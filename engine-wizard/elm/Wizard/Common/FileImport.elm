module Wizard.Common.FileImport exposing (Model, Msg, UpdateConfig, ViewConfig, initialModel, update, view)

import ActionResult exposing (ActionResult)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Gettext exposing (gettext)
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Json.Decode as D
import Shared.Api exposing (ToMsg)
import Shared.Error.ApiError as ApiError exposing (ApiError)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltipLeft)
import Wizard.Common.Html.Events exposing (alwaysPreventDefaultOn)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Flash as Flash
import Wizard.Routes exposing (Route)


type alias Model =
    { hover : Bool
    , files : Maybe (List File)
    , submitting : Dict String (ActionResult ())
    }


initialModel : Model
initialModel =
    { hover = False
    , files = Nothing
    , submitting = Dict.empty
    }


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | PickedFiles File (List File)
    | GotFiles (List File)
    | Upload
    | Cancel
    | SubmitComplete String (Result ApiError ())


type alias UpdateConfig msg =
    { mimes : List String
    , upload : File -> AppState -> ToMsg () msg -> Cmd msg
    , wrapMsg : Msg -> msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Pick ->
            ( model
            , Cmd.map cfg.wrapMsg <| Select.files cfg.mimes PickedFiles
            )

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
            ( { model | files = Just files }
            , Cmd.none
            )

        Upload ->
            case model.files of
                Just files ->
                    let
                        upload file ( actionResults, cmds ) =
                            case Dict.get (File.name file) actionResults of
                                Just (ActionResult.Success _) ->
                                    ( actionResults, cmds )

                                _ ->
                                    let
                                        fileName =
                                            File.name file

                                        cmd =
                                            cfg.upload file appState (cfg.wrapMsg << SubmitComplete fileName)
                                    in
                                    ( Dict.insert fileName ActionResult.Loading actionResults
                                    , cmd :: cmds
                                    )

                        ( submitting, uploadCmds ) =
                            List.foldl upload ( model.submitting, [] ) files
                    in
                    ( { model | submitting = submitting }, Cmd.batch uploadCmds )

                Nothing ->
                    ( model, Cmd.none )

        Cancel ->
            ( { model | files = Nothing, submitting = Dict.empty, hover = False }
            , Cmd.none
            )

        SubmitComplete fileName result ->
            case result of
                Ok _ ->
                    let
                        submitting =
                            Dict.insert fileName (ActionResult.Success ()) model.submitting
                    in
                    ( { model | submitting = submitting }
                    , Cmd.none
                    )

                Err error ->
                    let
                        submitting =
                            Dict.insert fileName (ApiError.toActionResult appState (gettext "Unable to upload file." appState.locale) error) model.submitting
                    in
                    ( { model | submitting = submitting }
                    , Cmd.none
                    )


type alias ViewConfig =
    { validate : Maybe (File -> Maybe String)
    , doneRoute : Route
    }


view : AppState -> ViewConfig -> Model -> Html Msg
view appState cfg model =
    case model.files of
        Just files ->
            filesView cfg appState model files

        Nothing ->
            dropzone appState model


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
    div
        [ class "dropzone"
        , classList [ ( "active", model.hover ) ]
        , alwaysPreventDefaultOn "dragenter" (D.succeed DragEnter)
        , alwaysPreventDefaultOn "dragover" (D.succeed DragEnter)
        , alwaysPreventDefaultOn "dragleave" (D.succeed DragLeave)
        , alwaysPreventDefaultOn "drop" dropDecoder
        , dataCy "dropzone"
        ]
        [ button [ onClick Pick, class "btn btn-secondary" ] [ text (gettext "Choose files" appState.locale) ]
        , p [] [ text (gettext "Or just drop them here" appState.locale) ]
        ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.map GotFiles (D.list File.decoder))


filesView : ViewConfig -> AppState -> Model -> List File -> Html Msg
filesView cfg appState model files =
    let
        fileIcon file =
            case Dict.get (File.name file) model.submitting of
                Just ActionResult.Loading ->
                    span [ class "text-muted" ] [ faSet "_global.spinner" appState ]

                Just (ActionResult.Success _) ->
                    span [ class "text-success" ] [ faSet "_global.success" appState ]

                Just (ActionResult.Error error) ->
                    span (class "text-danger" :: tooltipLeft error) [ faSet "_global.error" appState ]

                _ ->
                    case cfg.validate of
                        Just validate ->
                            case validate file of
                                Just validationError ->
                                    span (class "text-warning" :: tooltipLeft validationError)
                                        [ faSet "_global.warning" appState ]

                                Nothing ->
                                    Html.nothing

                        Nothing ->
                            Html.nothing

        fileView file =
            div [ class "rounded-3 bg-light d-flex mb-1 px-3 py-2", dataCy "file-import_file" ]
                [ span [ class "me-2" ] [ faSet "import.file" appState ]
                , span [ class "flex-grow-1 text-truncate" ] [ text (File.name file) ]
                , span [ class "ms-2" ] [ fileIcon file ]
                ]

        combinedResult =
            ActionResult.all (Dict.values model.submitting)

        globalResult =
            case combinedResult of
                ActionResult.Success _ ->
                    Flash.success appState (gettext "All files were uploaded successfully." appState.locale)

                ActionResult.Error _ ->
                    Flash.error appState (gettext "Unable to upload some files." appState.locale)

                _ ->
                    Html.nothing

        controls =
            if ActionResult.isSuccess combinedResult then
                div [ class "mt-4" ]
                    [ linkTo appState
                        cfg.doneRoute
                        [ class "btn btn-primary btn-wide"
                        , dataCy "file-import_done"
                        ]
                        [ text (gettext "Done" appState.locale) ]
                    ]

            else
                let
                    anySubmitting =
                        List.any ActionResult.isLoading (Dict.values model.submitting)
                in
                div [ class "form-actions" ]
                    [ button [ disabled anySubmitting, onClick Cancel, class "btn btn-secondary" ]
                        [ text (gettext "Cancel" appState.locale) ]
                    , ActionButton.button appState <| ActionButton.ButtonConfig (gettext "Import" appState.locale) combinedResult Upload False
                    ]
    in
    div [ class "rounded-3" ]
        [ globalResult
        , div [] (List.map fileView files)
        , controls
        ]
