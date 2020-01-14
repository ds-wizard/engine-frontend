module Wizard.KMEditor.Editor.KMEditor.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Keyed
import Maybe.Extra as Maybe
import Shared.Locale exposing (l)
import SplitPane exposing (ViewConfig, createViewConfig)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (emptyNode)
import Wizard.Common.View.Modal as Modal exposing (AlertConfig)
import Wizard.KMEditor.Editor.KMEditor.Components.MoveModal as MoveModal
import Wizard.KMEditor.Editor.KMEditor.Models exposing (..)
import Wizard.KMEditor.Editor.KMEditor.Msgs exposing (..)
import Wizard.KMEditor.Editor.KMEditor.View.Breadcrumbs exposing (breadcrumbs)
import Wizard.KMEditor.Editor.KMEditor.View.Editors exposing (activeEditor)
import Wizard.KMEditor.Editor.KMEditor.View.Tree exposing (treeView)


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.View"


view : AppState -> Model -> Html Msg
view appState model =
    let
        breadcrumbsView =
            case model.activeEditorUuid of
                Just activeUuid ->
                    breadcrumbs activeUuid model.editors

                _ ->
                    emptyNode

        moveModalViewProps =
            { editors = model.editors
            , kmUuid = model.knowledgeModel.uuid
            , movingUuid = Maybe.withDefault "" model.activeEditorUuid
            }
    in
    div [ class "KMEditor__Editor__KMEditor" ]
        [ div [ class "editor-breadcrumbs" ]
            [ breadcrumbsView ]
        , SplitPane.view viewConfig (viewTree appState model) (viewEditor appState model) model.splitPane
        , Modal.alert (alertConfig appState model)
        , MoveModal.view appState moveModalViewProps model.moveModal |> Html.map MoveModalMsg
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
