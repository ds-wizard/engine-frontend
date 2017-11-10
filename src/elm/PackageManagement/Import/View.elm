module PackageManagement.Import.View exposing (..)

import Common.View exposing (pageHeader)
import Common.View.Forms exposing (errorView)
import DragDrop exposing (onDragEnter, onDragLeave, onDragOver, onDrop)
import FileReader exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Msgs
import PackageManagement.Import.Models exposing (..)
import PackageManagement.Import.Msgs exposing (Msg(..))


view : Model -> Html Msgs.Msg
view model =
    let
        content =
            case List.head model.files of
                Just file ->
                    fileView file.name

                Nothing ->
                    dropzone model |> Html.map Msgs.PackageManagementImportMsg
    in
    div []
        [ pageHeader "Import package" []
        , errorView model.error
        , content
        ]


fileView : String -> Html Msgs.Msg
fileView fileName =
    div [ class "file-view" ]
        [ div []
            [ i [ class "fa fa-file-o" ] []
            , div [ class "filename" ]
                [ text fileName ]
            ]
        , div [ class "actions" ]
            [ button [ onClick (Msgs.PackageManagementImportMsg Cancel), class "btn btn-default" ]
                [ text "Cancel" ]
            , button [ onClick (Msgs.PackageManagementImportMsg Submit), class "btn btn-primary" ]
                [ text "Upload" ]
            ]
        ]


dropzone : Model -> Html Msg
dropzone model =
    div (dropzoneAttributes model)
        [ label [ class "btn btn-default btn-file" ]
            [ text "Choose file"
            , input [ type_ "file", onchange FilesSelect ] []
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
    class ("dropzone " ++ cssClass)
        :: [ onDragEnter DragEnter
           , onDragOver DragOver
           , onDragLeave DragLeave
           , onDrop Drop
           ]


onchange : (List NativeFile -> value) -> Attribute value
onchange action =
    on "change" (Decode.map action parseSelectedFiles)
