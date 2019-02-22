module KMEditor.Editor2.View exposing (view)

import ActionResult
import Common.Html exposing (fa)
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KMEditor.Common.Models exposing (Branch)
import KMEditor.Common.Models.Entities exposing (Level, Metric)
import KMEditor.Editor2.Models exposing (EditorType(..), Model)
import KMEditor.Editor2.Msgs exposing (Msg(..))
import KMEditor.Editor2.Preview.View
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
                    kmEditorView

                TagsEditor ->
                    tagsEditorView

                PreviewEditor ->
                    previewView wrapMsg model levels branch

                HistoryEditor ->
                    historyView
    in
    div [ class "KMEditor__Editor2" ]
        [ editorHeader wrapMsg model.currentEditor
        , div [ class "editor-body" ]
            [ Page.actionResultView content model.preview
            ]
        ]


editorHeader : (Msg -> Msgs.Msg) -> EditorType -> Html Msgs.Msg
editorHeader wrapMsg activeEditor =
    div [ class "editor-header" ]
        [ div [ class "undo" ]
            [ a [] [ fa "undo" ]
            , a [ class "disabled" ] [ fa "repeat" ]
            ]
        , ul [ class "nav" ]
            [ a
                [ class "nav-link"
                , classList [ ( "active", activeEditor == KMEditor ) ]
                , onClick <| wrapMsg <| OpenEditor KMEditor
                ]
                [ fa "sitemap", text "Knowledge Model" ]
            , a
                [ class "nav-link"
                , classList [ ( "active", activeEditor == TagsEditor ) ]
                , onClick <| wrapMsg <| OpenEditor TagsEditor
                ]
                [ fa "tags", text "Tags" ]
            , a
                [ class "nav-link"
                , classList [ ( "active", activeEditor == PreviewEditor ) ]
                , onClick <| wrapMsg <| OpenEditor PreviewEditor
                ]
                [ fa "eye", text "Preview" ]
            , a
                [ class "nav-link"
                , classList [ ( "active", activeEditor == HistoryEditor ) ]
                , onClick <| wrapMsg <| OpenEditor HistoryEditor
                ]
                [ fa "history", text "History" ]
            ]
        , div [ class "actions" ]
            [ button [ class "btn btn-primary btn-with-loader" ]
                [ text "Save" ]
            ]
        ]


kmEditorView : Html Msgs.Msg
kmEditorView =
    div [] [ text "Knowledge Model Editor" ]


tagsEditorView : Html Msgs.Msg
tagsEditorView =
    div [] [ text "Tags Editor" ]


previewView : (Msg -> Msgs.Msg) -> Model -> List Level -> Branch -> Html Msgs.Msg
previewView wrapMsg model levels branch =
    model.previewEditorModel
        |> Maybe.map (KMEditor.Editor2.Preview.View.view (wrapMsg << PreviewEditorMsg) levels)
        |> Maybe.withDefault (Page.error "Error opening preview")


historyView : Html Msgs.Msg
historyView =
    div [] [ text "History" ]
