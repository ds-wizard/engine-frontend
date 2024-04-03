module Wizard.KnowledgeModels.Import.OwlImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import File
import Form
import Gettext exposing (gettext)
import Html exposing (Attribute, Html, a, div, input, label, p, text)
import Html.Attributes exposing (class, disabled, id, type_)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Decode
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.KnowledgeModels.Import.OwlImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.file of
                Just file ->
                    fileView appState model (File.name file)

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
                    [ FormGroup.input appState model.form "name" (gettext "Name" appState.locale)
                    , FormGroup.input appState model.form "organizationId" (gettext "Organization ID" appState.locale)
                    , FormGroup.input appState model.form "kmId" (gettext "Knowledge Model ID" appState.locale)
                    , FormGroup.input appState model.form "version" (gettext "Version" appState.locale)
                    , FormGroup.input appState model.form "previousPackageId" (gettext "Previous Package ID" appState.locale)
                    , FormGroup.input appState model.form "rootElement" (gettext "Root Element" appState.locale)
                    ]

        formActions =
            FormActions.view appState
                Cancel
                (ActionButton.ButtonConfig (gettext "Import" appState.locale) model.importing (FormMsg <| Form.Submit) False)
    in
    div [ id dropzoneId, dataCy "import_file" ]
        [ FormResult.view appState model.importing
        , formView
        , fileGroup
        , formActions
        ]


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
    div [ class "file-view" ]
        [ div [ class "file" ]
            [ faSet "import.file" appState
            , div [ class "filename" ]
                [ text fileName
                , a [ disabled cancelDisabled, class "ms-1 text-danger", onClick CancelFile ]
                    [ faSet "_global.remove" appState ]
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
