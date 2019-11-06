module Wizard.KnowledgeModels.Import.FileImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (faSet)
import Wizard.Common.Locale exposing (l, lx)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.FormResult as FormResult
import Wizard.KnowledgeModels.Import.FileImport.Models exposing (..)
import Wizard.KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))


l_ : String -> AppState -> String
l_ =
    l "Wizard.KnowledgeModels.Import.FileImport.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KnowledgeModels.Import.FileImport.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        content =
            case model.file of
                Just file ->
                    fileView appState model file.filename

                Nothing ->
                    dropzone appState model
    in
    div [ class "KnowledgeModels__Import__FileImport", id dropzoneId ]
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
    div [ class "file-view" ]
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
