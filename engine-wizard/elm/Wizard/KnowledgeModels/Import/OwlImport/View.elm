module Wizard.KnowledgeModels.Import.OwlImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import File
import Form
import Html exposing (Attribute, Html, a, div, input, label, p, text)
import Html.Attributes exposing (class, disabled, id, type_)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Decode
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormActions as FormActions
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Common.View.FormResult as FormResult
import Wizard.KnowledgeModels.Import.OwlImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.KnowledgeModels.Import.OwlImport.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Import.OwlImport.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KnowledgeModels.Import.OwlImport.View"


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
                [ label [] [ lx_ "form.file" appState ]
                , content
                ]

        formView =
            Html.map FormMsg <|
                div []
                    [ FormGroup.input appState model.form "name" (l_ "form.name" appState)
                    , FormGroup.input appState model.form "organizationId" (l_ "form.organizationId" appState)
                    , FormGroup.input appState model.form "kmId" (l_ "form.kmId" appState)
                    , FormGroup.input appState model.form "version" (l_ "form.version" appState)
                    , FormGroup.input appState model.form "previousPackageId" (l_ "form.previousPackageId" appState)
                    , FormGroup.input appState model.form "rootElement" (l_ "form.rootElement" appState)
                    ]

        formActions =
            FormActions.view appState
                Routes.knowledgeModelsIndex
                (ActionButton.ButtonConfig (l_ "form.upload" appState) model.importing (FormMsg <| Form.Submit) False)
    in
    div [ class "KnowledgeModels__Import__FileImport", id dropzoneId, dataCy "km_import_file" ]
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
            [ faSet "kmImport.file" appState
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
            [ lx_ "dropzone.choose" appState
            , input [ id fileInputId, type_ "file", on "change" (Decode.succeed FileSelected) ] []
            ]
        , p [] [ lx_ "dropzone.drop" appState ]
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
