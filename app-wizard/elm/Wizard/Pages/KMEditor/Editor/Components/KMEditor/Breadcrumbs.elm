module Wizard.Pages.KMEditor.Editor.Components.KMEditor.Breadcrumbs exposing (view)

import Html exposing (Html, li, ol, text)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext as EditorContext exposing (EditorContext)
import Wizard.Routes as Routes


view : AppState -> EditorContext -> Html msg
view appState editorContext =
    let
        nodes =
            createBreadCrumbs appState editorContext maxBreadcrumbsNodeCount editorContext.activeUuid
                |> List.map (breadcrumbNode editorContext)
    in
    ol [ class "breadcrumb" ] nodes


maxBreadcrumbsNodeCount : Int
maxBreadcrumbsNodeCount =
    4


createBreadCrumbs : AppState -> EditorContext -> Int -> String -> List ( Maybe String, String )
createBreadCrumbs appState editorContext depth uuid =
    let
        node =
            if depth == maxBreadcrumbsNodeCount then
                ( Nothing, EditorContext.getEditorName appState uuid editorContext )

            else if depth == 0 then
                ( Nothing, "..." )

            else
                ( Just uuid, EditorContext.getEditorName appState uuid editorContext )
    in
    case ( EditorContext.getParentUuid uuid editorContext, depth ) of
        ( _, 0 ) ->
            [ node ]

        ( "", _ ) ->
            [ node ]

        ( parentUuid, 4 ) ->
            createBreadCrumbs appState editorContext (depth - 1) parentUuid
                ++ [ node ]

        ( parentUuid, _ ) ->
            createBreadCrumbs appState editorContext (depth - 1) parentUuid
                ++ [ node ]


breadcrumbNode : EditorContext -> ( Maybe String, String ) -> Html msg
breadcrumbNode editorContext ( maybeUuid, label ) =
    let
        ( content, withLink ) =
            case maybeUuid of
                Just uuid ->
                    ( linkTo (Routes.kmEditorEditor editorContext.kmEditor.uuid (EditorContext.getEditUuid uuid editorContext))
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
