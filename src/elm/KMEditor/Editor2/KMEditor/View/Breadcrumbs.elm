module KMEditor.Editor2.KMEditor.View.Breadcrumbs exposing (breadcrumbs)

import Common.Html exposing (emptyNode)
import Dict exposing (Dict)
import Html exposing (Html, a, li, ol, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KMEditor.Common.Models.Path exposing (getNodeUuid)
import KMEditor.Editor2.KMEditor.Models.Editors exposing (Editor, getEditorPath, getEditorTitle)
import KMEditor.Editor2.KMEditor.Msgs exposing (Msg(..))
import List.Extra as List


breadcrumbs : String -> Dict String Editor -> Html Msg
breadcrumbs activeUuid editors =
    case Dict.get activeUuid editors of
        Just editor ->
            let
                path =
                    getEditorPath editor

                nodes =
                    path
                        |> List.splitAt (List.length path - 4)
                        |> Tuple.second
                        |> List.map (getNodeUuid >> mapIntoLabel editors)
                        |> (\a -> List.append a [ ( Nothing, getEditorTitle editor ) ])
                        |> addTooLongNode (List.length path > 4)
                        |> List.map breadcrumbNode
            in
            ol [ class "breadcrumb" ]
                nodes

        Nothing ->
            emptyNode


breadcrumbNode : ( Maybe String, String ) -> Html Msg
breadcrumbNode ( maybeUuid, label ) =
    let
        ( content, withLink ) =
            case maybeUuid of
                Just uuid ->
                    ( a [ onClick <| SetActiveEditor uuid ] [ text label ], True )

                Nothing ->
                    ( text label, False )
    in
    li [ class "breadcrumb-item", classList [ ( "with-link", withLink ) ] ] [ content ]


mapIntoLabel : Dict String Editor -> String -> ( Maybe String, String )
mapIntoLabel editors uuid =
    case Dict.get uuid editors of
        Just editor ->
            ( Just uuid, getEditorTitle editor )

        Nothing ->
            ( Nothing, "-" )


addTooLongNode : Bool -> List ( Maybe String, String ) -> List ( Maybe String, String )
addTooLongNode tooLong list =
    if tooLong then
        [ ( Nothing, "..." ) ] ++ list

    else
        list
