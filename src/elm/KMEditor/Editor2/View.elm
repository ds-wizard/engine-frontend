module KMEditor.Editor2.View exposing (view)

import ActionResult
import Common.Html exposing (emptyNode, fa)
import Common.View.ActionButton as ActionButton
import Common.View.Flash as Flash
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KMEditor.Common.Models exposing (Branch)
import KMEditor.Common.Models.Entities exposing (Level, Metric)
import KMEditor.Editor2.KMEditor.View
import KMEditor.Editor2.Models exposing (EditorType(..), Model, containsChanges, getSavingError, hasSavingError)
import KMEditor.Editor2.Msgs exposing (Msg(..))
import KMEditor.Editor2.Preview.View
import KMEditor.Editor2.TagEditor.View
import Msgs


view : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
view wrapMsg model =
    Page.actionResultView (editorView wrapMsg model) <|
        ActionResult.combine3 model.branch model.metrics model.levels


editorView : (Msg -> Msgs.Msg) -> Model -> ( Branch, List Metric, List Level ) -> Html Msgs.Msg
editorView wrapMsg model ( branch, metric, levels ) =
    let
        content _ =
            case model.currentEditor of
                KMEditor ->
                    kmEditorView wrapMsg model

                TagsEditor ->
                    tagsEditorView wrapMsg model

                PreviewEditor ->
                    previewView wrapMsg model levels branch

                HistoryEditor ->
                    historyView
    in
    div [ class "KMEditor__Editor2" ]
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
                , ActionButton.button ( "Save", model.saving, wrapMsg Save )
                ]

            else
                []

        errorMsg =
            if hasSavingError model then
                Flash.error <| getSavingError model

            else
                emptyNode
    in
    div [ class "editor-header", classList [ ( "with-error", hasSavingError model ) ] ]
        [ div [ class "navigation" ]
            [ div [ class "undo" ]
                [ a [] [ fa "undo" ]
                , a [ class "disabled" ] [ fa "repeat" ]
                ]
            , ul [ class "nav" ]
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
                , a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == HistoryEditor ) ]
                    , onClick <| wrapMsg <| OpenEditor HistoryEditor
                    ]
                    [ fa "history", text "History" ]
                ]
            , div [ class "actions" ] actions
            ]
        , errorMsg
        ]


kmEditorView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
kmEditorView wrapMsg model =
    model.editorModel
        |> Maybe.map (KMEditor.Editor2.KMEditor.View.view (wrapMsg << KMEditorMsg))
        |> Maybe.withDefault (Page.error "Error opening knowledge model editor")


tagsEditorView : (Msg -> Msgs.Msg) -> Model -> Html Msgs.Msg
tagsEditorView wrapMsg model =
    model.tagEditorModel
        |> Maybe.map (KMEditor.Editor2.TagEditor.View.view (wrapMsg << TagEditorMsg))
        |> Maybe.withDefault (Page.error "Error opening tag editor")


previewView : (Msg -> Msgs.Msg) -> Model -> List Level -> Branch -> Html Msgs.Msg
previewView wrapMsg model levels branch =
    model.previewEditorModel
        |> Maybe.map (KMEditor.Editor2.Preview.View.view (wrapMsg << PreviewEditorMsg) levels)
        |> Maybe.withDefault (Page.error "Error opening preview")


historyView : Html Msgs.Msg
historyView =
    div [] [ text "History" ]
