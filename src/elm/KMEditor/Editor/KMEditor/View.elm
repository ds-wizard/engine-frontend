module KMEditor.Editor.KMEditor.View exposing (view)

import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode)
import Common.Locale exposing (l)
import Common.View.Modal as Modal exposing (AlertConfig)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Keyed
import KMEditor.Editor.KMEditor.Models exposing (..)
import KMEditor.Editor.KMEditor.Msgs exposing (..)
import KMEditor.Editor.KMEditor.View.Breadcrumbs exposing (breadcrumbs)
import KMEditor.Editor.KMEditor.View.Editors exposing (activeEditor)
import KMEditor.Editor.KMEditor.View.Tree exposing (treeView)
import Maybe.Extra as Maybe
import SplitPane exposing (ViewConfig, createViewConfig)


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Editor.KMEditor.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        breadcrumbsView =
            case model.activeEditorUuid of
                Just activeUuid ->
                    breadcrumbs activeUuid model.editors

                _ ->
                    emptyNode
    in
    div [ class "KMEditor__Editor__KMEditor" ]
        [ div [ class "editor-breadcrumbs" ]
            [ breadcrumbsView ]
        , SplitPane.view viewConfig (viewTree appState model) (viewEditor appState model) model.splitPane
        , Modal.alert (alertConfig appState model)
        ]


viewConfig : ViewConfig Msg
viewConfig =
    createViewConfig
        { toMsg = PaneMsg
        , customSplitter = Nothing
        }


viewTree : AppState -> Model -> Html Msg
viewTree appState model =
    div [ class "tree-col" ]
        [ treeView appState (Maybe.withDefault "" model.activeEditorUuid) model.editors model.knowledgeModel.uuid
        ]


viewEditor : AppState -> Model -> Html Msg
viewEditor appState model =
    Html.Keyed.node "div"
        [ class "editor-form-view", id "editor-view" ]
        [ activeEditor appState model
        ]


alertConfig : AppState -> Model -> AlertConfig Msg
alertConfig appState model =
    { message = Maybe.withDefault "" model.alert
    , visible = Maybe.isJust model.alert
    , actionMsg = CloseAlert
    , actionName = l_ "alert.close" appState
    }
