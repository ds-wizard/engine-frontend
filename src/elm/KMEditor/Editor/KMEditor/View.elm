module KMEditor.Editor.KMEditor.View exposing (view)

import Common.Html exposing (emptyNode)
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
import Msgs
import SplitPane exposing (ViewConfig, createViewConfig)


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    let
        breadcrumbsView =
            case model.activeEditorUuid of
                Just activeUuid ->
                    breadcrumbs activeUuid model.editors |> Html.map wrapMsg

                _ ->
                    emptyNode
    in
    div [ class "KMEditor__Editor__KMEditor" ]
        [ div [ class "editor-breadcrumbs" ]
            [ breadcrumbsView ]
        , SplitPane.view viewConfig (viewTree model) (viewEditor model) model.splitPane |> Html.map wrapMsg
        , Modal.alert (alertConfig model) |> Html.map wrapMsg
        ]


viewConfig : ViewConfig Msg
viewConfig =
    createViewConfig
        { toMsg = PaneMsg
        , customSplitter = Nothing
        }


viewTree : Model -> Html Msg
viewTree model =
    div [ class "tree-col" ]
        [ treeView (Maybe.withDefault "" model.activeEditorUuid) model.editors model.knowledgeModel.uuid
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
