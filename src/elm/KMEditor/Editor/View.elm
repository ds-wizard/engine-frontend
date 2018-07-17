module KMEditor.Editor.View exposing (..)

import Common.Html exposing (emptyNode)
import Common.View exposing (AlertConfig, alertView, fullPageActionResultView)
import Common.View.Forms exposing (actionButton)
import Html exposing (..)
import Html.Attributes exposing (class, classList, id)
import Html.Keyed
import KMEditor.Editor.Models exposing (..)
import KMEditor.Editor.Msgs exposing (..)
import KMEditor.Editor.View.Breadcrumbs exposing (breadcrumbs)
import KMEditor.Editor.View.Editors exposing (activeEditor)
import KMEditor.Editor.View.Tree exposing (treeView)
import Maybe.Extra as Maybe
import Msgs
import SplitPane exposing (ViewConfig, createViewConfig)


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

        unsavedChanges =
            if List.length model.events > 0 then
                text "(unsaved changes)"
            else
                emptyNode
    in
    div [ class "row" ]
        [ div [ class "editor-header" ]
            [ text "Knowledge Model Editor"
            , div []
                [ unsavedChanges
                , actionButton ( "Save", model.submitting, wrapMsg Submit )
                ]
            ]
        , div [ class "editor-breadcrumbs" ]
            [ breadcrumbsView ]
        , SplitPane.view viewConfig (viewTree model kmUuid) (viewEditor model) model.splitPane |> Html.map wrapMsg
        ]


viewConfig : ViewConfig Msg
viewConfig =
    createViewConfig
        { toMsg = PaneMsg
        , customSplitter = Nothing
        }


viewTree : Model -> String -> Html Msg
viewTree model kmUuid =
    div [ class "tree-col" ]
        [ treeView (Maybe.withDefault "" model.activeEditorUuid) model.editors kmUuid
        ]


viewEditor : Model -> Html Msg
viewEditor model =
    Html.Keyed.node "div"
        [ class "editor-form-view", id "editor-view" ]
        [ activeEditor model
        ]


alertConfig : Model -> AlertConfig Msg
alertConfig model =
    { message = Maybe.withDefault "" model.alert
    , visible = Maybe.isJust model.alert
    , actionMsg = CloseAlert
    , actionName = "Ok"
    }
