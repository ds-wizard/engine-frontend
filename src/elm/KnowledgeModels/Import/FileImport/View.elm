module KnowledgeModels.Import.FileImport.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.View.ActionButton as ActionButton
import Common.View.FormResult as FormResult
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import KnowledgeModels.Import.FileImport.Models exposing (..)
import KnowledgeModels.Import.FileImport.Msgs exposing (Msg(..))


view : Model -> Html Msg
view model =
    let
        content =
            case model.file of
                Just file ->
                    fileView model file.filename

                Nothing ->
                    dropzone model
    in
    div [ class "KnowledgeModels__Import__FileImport", id dropzoneId ]
        [ FormResult.view model.importing
        , content
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
            [ i [ class "fa fa-file-o" ] []
            , div [ class "filename" ]
                [ text fileName ]
            ]
        , div [ class "actions" ]
            [ button [ disabled cancelDisabled, onClick Cancel, class "btn btn-secondary" ]
                [ text "Cancel" ]
            , ActionButton.button <| ActionButton.ButtonConfig "Upload" model.importing Submit False
            ]
        ]


dropzone : Model -> Html Msg
dropzone model =
    div (dropzoneAttributes model)
        [ label [ class "btn btn-secondary btn-file" ]
            [ text "Choose file"
            , input [ id fileInputId, type_ "file", on "change" (Decode.succeed FileSelected) ] []
            ]
        , p [] [ text "or just drop it here" ]
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
