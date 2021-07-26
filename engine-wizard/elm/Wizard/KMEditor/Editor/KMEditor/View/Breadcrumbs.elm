module Wizard.KMEditor.Editor.KMEditor.View.Breadcrumbs exposing (breadcrumbs)

import Dict exposing (Dict)
import Html exposing (Html, a, li, ol, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.KMEditor.Editor.KMEditor.Models.Editors exposing (Editor, getEditorParentUuid, getEditorTitle, getEditorUuid)
import Wizard.KMEditor.Editor.KMEditor.Msgs exposing (Msg(..))


breadcrumbs : String -> String -> Dict String Editor -> Html Msg
breadcrumbs activeUuid kmName editors =
    case Dict.get activeUuid editors of
        Just editor ->
            let
                nodes =
                    getEditorUuid editor
                        |> createBreadCrumbs kmName editors maxBreadcrumbsNodeCount
                        |> List.map breadcrumbNode
            in
            ol [ class "breadcrumb" ]
                nodes

        Nothing ->
            emptyNode


maxBreadcrumbsNodeCount : Int
maxBreadcrumbsNodeCount =
    4


createBreadCrumbs : String -> Dict String Editor -> Int -> String -> List ( Maybe String, String )
createBreadCrumbs kmName editors depth editorUuid =
    case ( Dict.get editorUuid editors, depth ) of
        ( _, 0 ) ->
            [ ( Nothing, "..." ) ]

        ( Just editor, _ ) ->
            (createBreadCrumbs kmName editors (depth - 1) <| getEditorParentUuid editor)
                ++ [ ( Just editorUuid, getEditorTitle kmName editor ) ]

        ( Nothing, _ ) ->
            []


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
    li
        [ class "breadcrumb-item"
        , classList [ ( "with-link", withLink ) ]
        , dataCy "breadcrumb-item"
        ]
        [ content ]
