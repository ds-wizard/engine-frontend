module KMEditor.Editor.View exposing (view)

import ActionResult
import Common.AppState exposing (AppState)
import Common.Html exposing (emptyNode, fa, faSet, linkTo)
import Common.Locale exposing (l, lx)
import Common.View.ActionButton as ActionButton
import Common.View.Flash as Flash
import Common.View.Page as Page
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import KMEditor.Common.BranchDetail exposing (BranchDetail)
import KMEditor.Common.KnowledgeModel.Level exposing (Level)
import KMEditor.Common.KnowledgeModel.Metric exposing (Metric)
import KMEditor.Editor.KMEditor.View
import KMEditor.Editor.Models exposing (EditorType(..), Model, containsChanges, getSavingError, hasSavingError)
import KMEditor.Editor.Msgs exposing (Msg(..))
import KMEditor.Editor.Preview.View
import KMEditor.Editor.TagEditor.View
import KMEditor.Routes exposing (Route(..))
import Routes


l_ : String -> AppState -> String
l_ =
    l "KMEditor.Editor.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "KMEditor.Editor.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (editorView appState model) <|
        ActionResult.combine3 model.km model.metrics model.levels


editorView : AppState -> Model -> ( BranchDetail, List Metric, List Level ) -> Html Msg
editorView appState model ( _, _, levels ) =
    let
        content _ =
            case model.currentEditor of
                KMEditor ->
                    kmEditorView appState model

                TagsEditor ->
                    tagsEditorView appState model

                PreviewEditor ->
                    previewView appState model levels

                HistoryEditor ->
                    historyView
    in
    div [ class "KMEditor__Editor" ]
        [ editorHeader appState model
        , div [ class "editor-body", classList [ ( "with-error", hasSavingError model ) ] ]
            [ Page.actionResultView appState content model.preview
            ]
        ]


editorHeader : AppState -> Model -> Html Msg
editorHeader appState model =
    let
        actions =
            if containsChanges model then
                [ lx_ "header.unsavedChanges" appState
                , button [ onClick Discard, class "btn btn-outline-danger btn-with-loader" ]
                    [ lx_ "header.discard" appState ]
                , ActionButton.button <|
                    ActionButton.ButtonConfig (l_ "header.save" appState) model.saving Save False
                ]

            else
                [ linkTo appState
                    (Routes.KMEditorRoute IndexRoute)
                    [ class "btn btn-outline-primary btn-with-loader" ]
                    [ lx_ "header.close" appState ]
                ]

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
                    , onClick <| OpenEditor KMEditor
                    ]
                    [ faSet "kmEditor.knowledgeModel" appState, lx_ "nav.knowledgeModel" appState ]
                , a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == TagsEditor ) ]
                    , onClick <| OpenEditor TagsEditor
                    ]
                    [ faSet "kmEditor.tags" appState, lx_ "nav.tags" appState ]
                , a
                    [ class "nav-link"
                    , classList [ ( "active", model.currentEditor == PreviewEditor ) ]
                    , onClick <| OpenEditor PreviewEditor
                    ]
                    [ faSet "kmEditor.preview" appState, lx_ "nav.preview" appState ]

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


kmEditorView : AppState -> Model -> Html Msg
kmEditorView appState model =
    model.editorModel
        |> Maybe.map (Html.map KMEditorMsg << KMEditor.Editor.KMEditor.View.view appState)
        |> Maybe.withDefault (Page.error appState <| l_ "kmEditor.error" appState)


tagsEditorView : AppState -> Model -> Html Msg
tagsEditorView appState model =
    model.tagEditorModel
        |> Maybe.map (Html.map TagEditorMsg << KMEditor.Editor.TagEditor.View.view appState)
        |> Maybe.withDefault (Page.error appState <| l_ "tagsEditor.error" appState)


previewView : AppState -> Model -> List Level -> Html Msg
previewView appState model levels =
    model.previewEditorModel
        |> Maybe.map (Html.map PreviewEditorMsg << KMEditor.Editor.Preview.View.view appState levels)
        |> Maybe.withDefault (Page.error appState <| l_ "preview.error" appState)


historyView : Html Msg
historyView =
    div [] [ text "History" ]
