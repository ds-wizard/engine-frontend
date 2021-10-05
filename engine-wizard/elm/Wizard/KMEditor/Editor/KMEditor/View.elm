module Wizard.KMEditor.Editor.KMEditor.View exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Html.Keyed
import Maybe.Extra as Maybe
import Shared.Html exposing (emptyNode)
import Shared.Locale exposing (l)
import SplitPane exposing (ViewConfig, createViewConfig)
import Uuid
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Modal as Modal exposing (AlertConfig)
import Wizard.KMEditor.Editor.KMEditor.Components.MoveModal as MoveModal
import Wizard.KMEditor.Editor.KMEditor.Models exposing (Model)
import Wizard.KMEditor.Editor.KMEditor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Editor.KMEditor.View.Breadcrumbs exposing (breadcrumbs)
import Wizard.KMEditor.Editor.KMEditor.View.Editors exposing (activeEditor)
import Wizard.KMEditor.Editor.KMEditor.View.Tree exposing (treeView)


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.KMEditor.View"


view : AppState -> String -> Model -> Html Msg
view appState kmName model =
    let
        breadcrumbsView =
            case model.activeEditorUuid of
                Just activeUuid ->
                    breadcrumbs activeUuid kmName model.editors

                _ ->
                    emptyNode

        moveModalViewProps =
            { editors = model.editors
            , kmUuid = Uuid.toString model.knowledgeModel.uuid
            , kmName = kmName
            , movingUuid = Maybe.withDefault "" model.activeEditorUuid
            }
    in
    div [ class "KMEditor__Editor__KMEditor", dataCy "km-editor_km" ]
        [ div [ class "editor-breadcrumbs" ]
            [ breadcrumbsView ]
        , SplitPane.view viewConfig (viewTree appState kmName model) (viewEditor appState kmName model) model.splitPane
        , Modal.alert (alertConfig appState model)
        , MoveModal.view appState moveModalViewProps model.moveModal |> Html.map MoveModalMsg
        ]


viewConfig : ViewConfig Msg
viewConfig =
    createViewConfig
        { toMsg = PaneMsg
        , customSplitter = Nothing
        }


viewTree : AppState -> String -> Model -> Html Msg
viewTree appState kmName model =
    let
        cfg =
            { activeUuid = Maybe.withDefault "" model.activeEditorUuid
            , editors = model.editors
            , kmName = kmName
            }
    in
    div [ class "tree-col" ]
        [ treeView appState cfg (Uuid.toString model.knowledgeModel.uuid)
        ]


viewEditor : AppState -> String -> Model -> Html Msg
viewEditor appState kmName model =
    Html.Keyed.node "div"
        [ class "editor-form-view", id "editor-view" ]
        [ activeEditor appState kmName model
        ]


alertConfig : AppState -> Model -> AlertConfig Msg
alertConfig appState model =
    { message = Maybe.withDefault "" model.alert
    , visible = Maybe.isJust model.alert
    , actionMsg = CloseAlert
    , actionName = l_ "alert.close" appState
    }
