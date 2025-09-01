module Wizard.KMEditor.Editor.Components.KMEditor.Breadcrumbs exposing (view)

import Html exposing (Html, li, ol, text)
import Html.Attributes exposing (class, classList)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.KMEditor.Editor.Common.EditorBranch as EditorBranch exposing (EditorBranch)
import Wizard.Routes as Routes


view : AppState -> EditorBranch -> Html msg
view appState editorBranch =
    let
        nodes =
            createBreadCrumbs appState editorBranch maxBreadcrumbsNodeCount editorBranch.activeUuid
                |> List.map (breadcrumbNode editorBranch)
    in
    ol [ class "breadcrumb" ] nodes


maxBreadcrumbsNodeCount : Int
maxBreadcrumbsNodeCount =
    4


createBreadCrumbs : AppState -> EditorBranch -> Int -> String -> List ( Maybe String, String )
createBreadCrumbs appState editorBranch depth uuid =
    let
        node =
            if depth == maxBreadcrumbsNodeCount then
                ( Nothing, EditorBranch.getEditorName appState uuid editorBranch )

            else if depth == 0 then
                ( Nothing, "..." )

            else
                ( Just uuid, EditorBranch.getEditorName appState uuid editorBranch )
    in
    case ( EditorBranch.getParentUuid uuid editorBranch, depth ) of
        ( _, 0 ) ->
            [ node ]

        ( "", _ ) ->
            [ node ]

        ( parentUuid, 4 ) ->
            createBreadCrumbs appState editorBranch (depth - 1) parentUuid
                ++ [ node ]

        ( parentUuid, _ ) ->
            createBreadCrumbs appState editorBranch (depth - 1) parentUuid
                ++ [ node ]


breadcrumbNode : EditorBranch -> ( Maybe String, String ) -> Html msg
breadcrumbNode editorBranch ( maybeUuid, label ) =
    let
        ( content, withLink ) =
            case maybeUuid of
                Just uuid ->
                    ( linkTo (Routes.kmEditorEditor editorBranch.branch.uuid (EditorBranch.getEditUuid uuid editorBranch))
                        []
                        [ text label ]
                    , True
                    )

                Nothing ->
                    ( text label, False )
    in
    li
        [ class "breadcrumb-item"
        , classList [ ( "with-link", withLink ) ]
        , dataCy "breadcrumb-item"
        ]
        [ content ]
