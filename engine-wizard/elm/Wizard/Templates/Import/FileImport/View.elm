module Wizard.Templates.Import.FileImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import File
import Html exposing (Attribute, Html, button, div, input, label, p, text)
import Html.Attributes exposing (class, disabled, id, type_)
import Html.Events exposing (custom, on, onClick)
import Json.Decode as Decode
import Shared.Html exposing (faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.Templates.Import.FileImport.Models exposing (Model, dropzoneId, fileInputId)
import Wizard.Templates.Import.FileImport.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.Templates.Import.FileImport.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Templates.Import.FileImport.View"


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
    div [ class "KnowledgeModels__Import__FileImport", id dropzoneId, dataCy "template_import_file" ]
        [ FormResult.view appState model.importing
        , content
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
    div [ class "file-view rounded-3" ]
        [ div [ class "file" ]
            [ faSet "kmImport.file" appState
            , div [ class "filename" ]
                [ text fileName ]
            ]
        , div [ class "actions" ]
            [ button [ disabled cancelDisabled, onClick Cancel, class "btn btn-secondary" ]
                [ lx_ "fileView.cancel" appState ]
            , ActionButton.button appState <| ActionButton.ButtonConfig (l_ "fileView.upload" appState) model.importing Submit False
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
