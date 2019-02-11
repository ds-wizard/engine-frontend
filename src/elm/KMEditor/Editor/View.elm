module KMEditor.Editor.View exposing (alertConfig, editorView, view, viewConfig, viewEditor, viewTree)

import ActionResult
import Common.Html exposing (emptyNode)
import Common.View.Forms exposing (actionButton, formErrorResultView)
import Common.View.Modal as Modal exposing (AlertConfig)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Html.Keyed
import KMEditor.Common.Models.Entities exposing (Level, Metric)
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
        [ Page.actionResultView (editorView wrapMsg model) (ActionResult.combine3 model.kmUuid model.metrics model.levels)
        , Modal.alert (alertConfig model) |> Html.map wrapMsg
        ]


editorView : (Msg -> Msgs.Msg) -> Model -> ( String, List Metric, List Level ) -> Html Msgs.Msg
editorView wrapMsg model ( kmUuid, _, _ ) =
    let
        breadcrumbsView =
            case model.activeEditorUuid of
                Just activeUuid ->
                    breadcrumbs activeUuid model.editors |> Html.map wrapMsg

                _ ->
                    emptyNode

        unsavedChanges =
            if containsChanges model then
                div []
                    [ text "(unsaved changes)"
                    , button [ onClick <| wrapMsg Discard, class "btn btn-secondary btn-with-loader" ] [ text "Discard" ]
                    , actionButton ( "Save", model.submitting, wrapMsg Submit )
                    ]

            else
                emptyNode
    in
    div [ class "row" ]
        [ div [ class "editor-header" ]
            [ text "Knowledge Model Editor"
            , formErrorResultView model.submitting
            , unsavedChanges
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
