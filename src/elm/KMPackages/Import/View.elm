module KMPackages.Import.View exposing (view)

import Common.Html exposing (detailContainerClassWith)
import Common.Types exposing (ActionResult(..))
import Common.View exposing (pageHeader)
import Common.View.Forms exposing (actionButton, formResultView)
import DragDrop exposing (onDragEnter, onDragLeave, onDragOver, onDrop)
import FileReader exposing (..)
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
            case List.head model.files of
                Just file ->
                    fileView wrapMsg model file.name

                Nothing ->
                    dropzone model |> Html.map wrapMsg
    in
    div [ detailContainerClassWith "package-management-import" ]
        [ pageHeader "Import package" []
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
            [ button [ disabled cancelDisabled, onClick (wrapMsg Cancel), class "btn btn-default" ]
                [ text "Cancel" ]
            , actionButton ( "Upload", model.importing, wrapMsg Submit )
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
