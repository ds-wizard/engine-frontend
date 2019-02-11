module KMPackages.Import.View exposing (view)

import ActionResult exposing (ActionResult(..))
import Common.Html exposing (detailContainerClassWith)
import Common.View.Forms exposing (actionButton, formResultView)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import KMPackages.Import.Models exposing (..)
import KMPackages.Import.Msgs exposing (Msg(..))
import Msgs


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    let
        content =
            case model.file of
                Just file ->
                    fileView wrapMsg model file.filename

                Nothing ->
                    dropzone model |> Html.map wrapMsg
    in
    div [ detailContainerClassWith "KMPackages__Import", id dropzoneId ]
        [ Page.header "Import Knowledge Model" []
        , formResultView model.importing
        , content
        ]


fileView : (Msg -> Msgs.Msg) -> Model -> String -> Html Msgs.Msg
fileView wrapMsg model fileName =
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
            [ button [ disabled cancelDisabled, onClick (wrapMsg Cancel), class "btn btn-secondary" ]
                [ text "Cancel" ]
            , actionButton ( "Upload", model.importing, wrapMsg Submit )
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
