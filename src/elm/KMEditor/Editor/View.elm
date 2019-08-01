module KMEditor.Editor.View exposing (view)

import ActionResult
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa, linkTo)
import Common.View.ActionButton as ActionButton
import Common.View.Flash as Flash
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KMEditor.Common.BranchDetail exposing (BranchDetail)
import KMEditor.Common.Models.Entities exposing (Level, Metric)
import KMEditor.Editor.KMEditor.View
import KMEditor.Editor.Models exposing (EditorType(..), Model, containsChanges, getSavingError, hasSavingError)
import KMEditor.Editor.Msgs exposing (Msg(..))
import KMEditor.Editor.Preview.View
import KMEditor.Editor.TagEditor.View
import KMEditor.Routing exposing (Route(..))
import Msgs
import Routing


view : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
view wrapMsg appState model =
    Page.actionResultView (editorView wrapMsg appState model) <|
        ActionResult.combine3 model.km model.metrics model.levels


editorView : (Msg -> Msgs.Msg) -> AppState -> Model -> ( BranchDetail, List Metric, List Level ) -> Html Msgs.Msg
editorView wrapMsg appState model ( _, _, levels ) =
    let
        content _ =
            case model.currentEditor of
                KMEditor ->
                    kmEditorView wrapMsg appState model

                TagsEditor ->
                    tagsEditorView wrapMsg model

                PreviewEditor ->
                    previewView wrapMsg appState model levels

                HistoryEditor ->
                    historyView
    in
    div [ class "KMEditor__Editor" ]
        [ editorHeader wrapMsg model
        , div [ class "editor-body", classList [ ( "with-error", hasSavingError model ) ] ]
            [ Page.actionResultView content model.preview
            ]
        ]


editorHeader : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
editorHeader wrapMsg model =
    let
        actions =
            if containsChanges model then
                [ text "(unsaved changes)"
                , button [ onClick <| wrapMsg Discard, class "btn btn-outline-danger btn-with-loader" ] [ text "Discard" ]
                , ActionButton.button <| ActionButton.ButtonConfig "Save" model.saving (wrapMsg Save) False
                ]

            else
                [ linkTo (Routing.KMEditor IndexRoute) [ class "btn btn-outline-primary btn-with-loader" ] [ text "Close" ] ]

        errorMsg =
            if hasSavingError model then
                Flash.error <| getSavingError model

            else
                emptyNode
    in
    div [ class "editor-header", classList [ ( "with-error", hasSavingError model ) ] ]
        [ div [ class "navigation" ]
            [ --            div [ class "undo" ]
              --                [ a [] [ fa "undo" ]
              --                , a [ class "disabled" ] [ fa "repeat" ]
              --                ]
              --            ,
              ul [ class "nav" ]
                [ a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == KMEditor ) ]
                    , onClick <| wrapMsg <| OpenEditor KMEditor
                    ]
                    [ fa "sitemap", text "Knowledge Model" ]
                , a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == TagsEditor ) ]
                    , onClick <| wrapMsg <| OpenEditor TagsEditor
                    ]
                    [ fa "tags", text "Tags" ]
                , a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == PreviewEditor ) ]
                    , onClick <| wrapMsg <| OpenEditor PreviewEditor
                    ]
                    [ fa "eye", text "Preview" ]

                --                , a
                --                    [ class "nav-link"
                --                    , classList [ ( "active", model.currentEditor == HistoryEditor ) ]
                --                    , onClick <| wrapMsg <| OpenEditor HistoryEditor
                --                    ]
                --                    [ fa "history", text "History" ]
                ]
            , div [ class "actions" ] actions
            ]
        , errorMsg
        ]


kmEditorView : (Msg -> Msgs.Msg) -> AppState -> Model -> Html Msgs.Msg
kmEditorView wrapMsg appState model =
    model.editorModel
        |> Maybe.map (KMEditor.Editor.KMEditor.View.view (wrapMsg << KMEditorMsg) appState)
        |> Maybe.withDefault (Page.error "Error opening knowledge model editor")


tagsEditorView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
tagsEditorView wrapMsg model =
    model.tagEditorModel
        |> Maybe.map (KMEditor.Editor.TagEditor.View.view (wrapMsg << TagEditorMsg))
        |> Maybe.withDefault (Page.error "Error opening tag editor")


previewView : (Msg -> Msgs.Msg) -> AppState -> Model -> List Level -> Html Msgs.Msg
previewView wrapMsg appState model levels =
    model.previewEditorModel
        |> Maybe.map (KMEditor.Editor.Preview.View.view (wrapMsg << PreviewEditorMsg) appState levels)
        |> Maybe.withDefault (Page.error "Error opening preview")


historyView : Html Msgs.Msg
historyView =
    div [] [ text "History" ]
