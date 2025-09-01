module Wizard.Pages.KnowledgeModels.Import.OwlImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import File
import Form
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, div, input, label, p, text)
import Html.Attributes exposing (class, disabled, id, type_)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Decode
import Shared.Components.ActionButton as ActionButton
import Shared.Components.FontAwesome exposing (faImportFile, faRemove)
import Shared.Components.FormGroup as FormGroup
import Shared.Components.FormResult as FormResult
import Wizard.Components.FormActions as FormActions
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KnowledgeModels.Import.OwlImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.Pages.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.file of
                Just file ->
                    fileView model (File.name file)

                Nothing ->
                    dropzone appState model

        fileGroup =
            div [ class "form-group" ]
                [ label [] [ text (gettext "File" appState.locale) ]
                , content
                ]

        formView =
            Html.map FormMsg <|
                div []
                    [ FormGroup.input appState.locale model.form "name" (gettext "Name" appState.locale)
                    , FormGroup.input appState.locale model.form "organizationId" (gettext "Organization ID" appState.locale)
                    , FormGroup.input appState.locale model.form "kmId" (gettext "Knowledge Model ID" appState.locale)
                    , FormGroup.input appState.locale model.form "version" (gettext "Version" appState.locale)
                    , FormGroup.input appState.locale model.form "previousPackageId" (gettext "Previous Package ID" appState.locale)
                    , FormGroup.input appState.locale model.form "rootElement" (gettext "Root Element" appState.locale)
                    ]

        formActions =
            FormActions.view appState
                Cancel
                (ActionButton.ButtonConfig (gettext "Import" appState.locale) model.importing (FormMsg <| Form.Submit) False)
    in
    div [ id dropzoneId, dataCy "import_file" ]
        [ FormResult.view model.importing
        , formView
        , fileGroup
        , formActions
        ]


fileView : Model -> String -> Html Msg
fileView model fileName =
    let
        cancelDisabled =
            case model.importing of
                Loading ->
                    True

                _ ->
                    False
    in
    div [ class "file-view" ]
        [ div [ class "file" ]
            [ faImportFile
            , div [ class "filename" ]
                [ text fileName
                , a [ disabled cancelDisabled, class "ms-1 text-danger", onClick CancelFile ]
                    [ faRemove ]
                ]
            ]
        ]


dropzone : AppState -> Model -> Html Msg
dropzone appState model =
    div (dropzoneAttributes model)
        [ label [ class "btn btn-secondary btn-file" ]
            [ text (gettext "Choose a file" appState.locale)
            , input [ id fileInputId, type_ "file", on "change" (Decode.succeed FileSelected) ] []
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
    [ class ("dropzone " ++ cssClass)
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
