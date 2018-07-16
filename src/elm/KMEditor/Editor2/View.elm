module KMEditor.Editor2.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.View exposing (AlertConfig, alertView, fullPageActionResultView)
import Common.View.Forms exposing (actionButton)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Keyed
import KMEditor.Editor2.Models exposing (..)
import KMEditor.Editor2.Msgs exposing (..)
import KMEditor.Editor2.View.Breadcrumbs exposing (breadcrumbs)
import KMEditor.Editor2.View.Editors exposing (activeEditor)
import KMEditor.Editor2.View.Tree exposing (treeView)
import Maybe.Extra as Maybe
import Msgs


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    div [ class "col KMEditor__Editor" ]
        [ fullPageActionResultView (editorView wrapMsg model) model.kmUuid
        , alertView (alertConfig model) |> Html.map wrapMsg
        ]


editorView : (Msg -> Msgs.Msg) -> Model -> String -> Html Msgs.Msg
editorView wrapMsg model kmUuid =
    let
        breadcrumbsView =
            case model.activeEditorUuid of
                Just activeUuid ->
                    breadcrumbs activeUuid model.editors |> Html.map wrapMsg

                _ ->
                    emptyNode
    in
    div [ class "row" ]
        [ div [ class "editor-header" ]
            [ text "Knowledge Model Editor"
            , actionButton ( "Save", model.submitting, wrapMsg Submit )
            ]
        , div [ class "editor-breadcrumbs" ]
            [ breadcrumbsView ]
        , div [ class "tree-col" ]
            [ treeView (Maybe.withDefault "" model.activeEditorUuid) model.editors kmUuid
            ]
            |> Html.map wrapMsg
        , Html.Keyed.node "div"
            [ class "editor-form-view" ]
            [ activeEditor model
            ]
            |> Html.map wrapMsg
        ]


alertConfig : Model -> AlertConfig Msg
alertConfig model =
    { message = Maybe.withDefault "" model.alert
    , visible = Maybe.isJust model.alert
    , actionMsg = CloseAlert
    , actionName = "Ok"
    }
