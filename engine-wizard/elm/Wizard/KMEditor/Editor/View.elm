module Wizard.KMEditor.Editor.View exposing (view)

import ActionResult
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Shared.Data.BranchDetail exposing (BranchDetail)
import Shared.Data.KnowledgeModel.Metric exposing (Metric)
import Shared.Data.KnowledgeModel.Phase exposing (Phase)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.View.ActionButton as ActionButton
import Wizard.Common.View.Flash as Flash
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Editor.KMEditor.View
import Wizard.KMEditor.Editor.Models exposing (EditorType(..), Model, containsChanges, getSavingError, hasSavingError)
import Wizard.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Editor.Preview.View
import Wizard.KMEditor.Editor.Settings.View
import Wizard.KMEditor.Editor.TagEditor.View
import Wizard.KMEditor.Routes exposing (Route(..))
import Wizard.Routes as Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.View"


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (editorView appState model) model.km


editorView : AppState -> Model -> BranchDetail -> Html Msg
editorView appState model _ =
    let
        content _ =
            case model.currentEditor of
                KMEditor ->
                    kmEditorView appState model

                TagsEditor ->
                    tagsEditorView appState model

                PreviewEditor ->
                    previewView appState model

                HistoryEditor ->
                    historyView

                SettingsEditor ->
                    settingsView appState model
    in
    div [ class "KMEditor__Editor" ]
        [ editorHeader appState model
        , div [ class "editor-body" ]
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
                , ActionButton.button appState <|
                    ActionButton.ButtonConfig (l_ "header.save" appState) model.saving Save False
                ]

            else
                [ linkTo appState
                    Routes.kmEditorIndex
                    [ class "btn btn-outline-primary btn-with-loader" ]
                    [ lx_ "header.close" appState ]
                ]

        kmName =
            ActionResult.unwrap "" .name model.km

        errorMsg =
            if hasSavingError model then
                span [ class "text-danger error" ] [ text <| getSavingError model ]

            else
                emptyNode
    in
    div [ class "DetailNavigation" ]
        [ div [ class "DetailNavigation__Row" ]
            [ div [ class "DetailNavigation__Row__Section" ]
                [ div [ class "title" ] [ text kmName ]
                ]
            , div [ class "DetailNavigation__Row__Section" ]
                [ div [ class "DetailNavigation__Row__Section__Actions" ] (errorMsg :: actions)
                ]
            ]
        , div [ class "DetailNavigation__Row" ]
            [ ul [ class "nav nav-underline-tabs" ]
                [ li [ class "nav-item" ]
                    [ a
                        [ class "nav-link"
                        , classList [ ( "active", model.currentEditor == KMEditor ) ]
                        , onClick <| OpenEditor KMEditor
                        ]
                        [ faSet "kmEditor.knowledgeModel" appState, lx_ "nav.knowledgeModel" appState ]
                    ]
                , li [ class "nav-item" ]
                    [ a
                        [ class "nav-link"
                        , classList [ ( "active", model.currentEditor == TagsEditor ) ]
                        , onClick <| OpenEditor TagsEditor
                        ]
                        [ faSet "kmEditor.tags" appState, lx_ "nav.tags" appState ]
                    ]
                , li [ class "nav-item" ]
                    [ a
                        [ class "nav-link"
                        , classList [ ( "active", model.currentEditor == PreviewEditor ) ]
                        , onClick <| OpenEditor PreviewEditor
                        ]
                        [ faSet "kmEditor.preview" appState, lx_ "nav.preview" appState ]
                    ]
                , li [ class "nav-item" ]
                    [ a
                        [ class "nav-link"
                        , classList [ ( "active", model.currentEditor == SettingsEditor ) ]
                        , onClick <| OpenEditor SettingsEditor
                        ]
                        [ faSet "kmEditor.settings" appState, lx_ "nav.settings" appState ]
                    ]
                ]
            ]
        ]


kmEditorView : AppState -> Model -> Html Msg
kmEditorView appState model =
    let
        kmName =
            ActionResult.unwrap "" .name model.km
    in
    model.editorModel
        |> Maybe.map (Html.map KMEditorMsg << Wizard.KMEditor.Editor.KMEditor.View.view appState kmName)
        |> Maybe.withDefault (Page.error appState <| l_ "kmEditor.error" appState)


tagsEditorView : AppState -> Model -> Html Msg
tagsEditorView appState model =
    model.tagEditorModel
        |> Maybe.map (Html.map TagEditorMsg << Wizard.KMEditor.Editor.TagEditor.View.view appState)
        |> Maybe.withDefault (Page.error appState <| l_ "tagsEditor.error" appState)


previewView : AppState -> Model -> Html Msg
previewView appState model =
    model.previewEditorModel
        |> Maybe.map (Html.map PreviewEditorMsg << Wizard.KMEditor.Editor.Preview.View.view appState)
        |> Maybe.withDefault (Page.error appState <| l_ "preview.error" appState)


historyView : Html Msg
historyView =
    div [] [ text "History" ]


settingsView : AppState -> Model -> Html Msg
settingsView appState model =
    Html.map SettingsFormMsg <|
        Wizard.KMEditor.Editor.Settings.View.view appState model.kmForm
