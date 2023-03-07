module Wizard.Locales.Import.FileImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import File
import File.Extra as File
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, button, div, input, label, p, text)
import Html.Attributes exposing (accept, class, disabled, id, type_)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Decode
import Shared.Html exposing (emptyNode, faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.FormResult as FormResult
import Wizard.Locales.Import.FileImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.Locales.Import.FileImport.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.file of
                Just file ->
                    fileView appState model (File.name file)

                Nothing ->
                    dropzone appState model
    in
    div [ id dropzoneId, dataCy "locale_import_file" ]
        [ FormResult.view appState model.importing
        , warningView appState model
        , content
        ]


warningView : AppState -> Model -> Html msg
warningView appState model =
    case ( model.importing, model.file ) of
        ( Unset, Just file ) ->
            if File.mime file == "application/zip" || File.ext file == Just "zip" then
                emptyNode

            else
                Flash.warning appState (gettext "This doesn't look like locale. Are you sure you picked the correct file?" appState.locale)

        _ ->
            emptyNode


fileView : AppState -> Model -> String -> Html Msg
fileView appState model fileName =
    let
        cancelDisabled =
            case model.importing of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "file-view rounded-3" ]
        [ div [ class "file" ]
            [ faSet "import.file" appState
            , div [ class "filename" ]
                [ text fileName ]
            ]
        , div [ class "actions" ]
            [ button [ disabled cancelDisabled, onClick Cancel, class "btn btn-secondary" ]
                [ text (gettext "Cancel" appState.locale) ]
            , ActionButton.button appState <| ActionButton.ButtonConfig (gettext "Import" appState.locale) model.importing Submit False
            ]
        ]


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
    div (dropzoneAttributes model)
        [ label [ class "btn btn-secondary btn-file" ]
            [ text (gettext "Choose a file" appState.locale)
            , input [ id fileInputId, type_ "file", on "change" (Decode.succeed FileSelected), accept ".zip" ] []
            ]
        , p [] [ text (gettext "Or just drop it here" appState.locale) ]
        ]


dropzoneAttributes : Model -> List (Attribute Msg)
dropzoneAttributes model =
    let
        cssClass =
            case model.dnd of
                0 ->
                    ""

                _ ->
                    "active"
    in
    [ class ("rounded-3 dropzone " ++ cssClass)
    , id dropzoneId
    , onDragEvent "dragenter" DragEnter
    , onDragEvent "dragover" DragOver
    , onDragEvent "dragleave" DragLeave
    ]


onDragEvent : String -> Msg -> Attribute Msg
onDragEvent event msg =
    custom event <|
        Decode.succeed
            { stopPropagation = True
            , preventDefault = True
            , message = msg
            }
