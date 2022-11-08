module Wizard.DocumentTemplateEditors.Editor.View exposing (view)

import ActionResult
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList)
import Html.Events exposing (onClick)
import Shared.Data.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Shared.Html exposing (faSet)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Components.DetailNavigation as DetailNavigation
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.ActionButton as ActionResult
import Wizard.Common.View.Page as Page
import Wizard.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.DocumentTemplateEditors.Editor.Components.TemplateEditor as TemplateEditor
import Wizard.DocumentTemplateEditors.Editor.Models exposing (CurrentEditor(..), Model, containsChanges)
import Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))


view : AppState -> Model -> Html Msg
view appState model =
    Page.actionResultView appState (viewTemplateEditor appState model) model.documentTemplate


viewTemplateEditor : AppState -> Model -> DocumentTemplateDraftDetail -> Html Msg
viewTemplateEditor appState model documentTemplate =
    let
        content =
            case model.currentEditor of
                TemplateEditor ->
                    Html.map TemplateEditorMsg <|
                        TemplateEditor.view appState model.templateEditorModel

                FilesEditor ->
                    let
                        viewConfig =
                            { documentTemplate = documentTemplate }
                    in
                    Html.map FileEditorMsg <|
                        FileEditor.view viewConfig appState model.fileEditorModel

                PreviewEditor ->
                    let
                        viewConfig =
                            { documentTemplate = documentTemplate }
                    in
                    Html.map PreviewMsg <|
                        Preview.view viewConfig appState model.previewModel

        publishModalViewConfig =
            { documentTemplate = documentTemplate }
    in
    div [ class "DocumentTemplateEditor col-full flex-column" ]
        [ viewEditorNavigation appState model
        , content
        , Html.map PublishModalMsg <| PublishModal.view publishModalViewConfig appState model.publishModalModel
        ]


viewEditorNavigation : AppState -> Model -> Html Msg
viewEditorNavigation appState model =
    let
        rightSection =
            if containsChanges model then
                let
                    saveButton =
                        ActionResult.buttonWithAttrs appState
                            { label = gettext "Save" appState.locale
                            , result =
                                ActionResult.all
                                    (ActionResult.map (always ()) model.templateEditorModel.savingForm
                                        :: Dict.values model.fileEditorModel.savingFiles
                                    )
                            , msg = Save
                            , dangerous = False
                            , attrs = [ dataCy "dt-editor_save" ]
                            }
                in
                [ span [ class "me-2" ]
                    [ text "("
                    , text (gettext "unsaved changes" appState.locale)
                    , text ")"
                    ]
                , saveButton
                ]

            else
                [ button
                    [ class "btn btn-primary with-icon"
                    , onClick (PublishModalMsg PublishModal.openMsg)
                    , dataCy "dt-editor_publish"
                    ]
                    [ faSet "documentTemplateEditor.publish" appState
                    , text (gettext "Publish" appState.locale)
                    ]
                ]

        templateName =
            ActionResult.unwrap "" .name model.documentTemplate
    in
    DetailNavigation.container
        [ DetailNavigation.row
            [ DetailNavigation.section
                [ div [ class "title" ] [ text templateName ]
                ]
            , DetailNavigation.sectionActions rightSection
            ]
        , viewEditorNavigationNav appState model
        ]


viewEditorNavigationNav : AppState -> Model -> Html Msg
viewEditorNavigationNav appState model =
    let
        templateLink =
            { label = gettext "Template" appState.locale
            , icon = faSet "documentTemplateEditor.template" appState
            , editor = TemplateEditor
            , dataCy = "dt-editor_nav_template"
            }

        filesLink =
            { label = gettext "Files" appState.locale
            , icon = faSet "documentTemplateEditor.files" appState
            , editor = FilesEditor
            , dataCy = "dt-editor_nav_files"
            }

        previewLink =
            { label = gettext "Preview" appState.locale
            , icon = faSet "documentTemplateEditor.preview" appState
            , editor = PreviewEditor
            , dataCy = "dt-editor_nav_preview"
            }

        viewLink link =
            li [ class "nav-item" ]
                [ a
                    [ class "nav-link"
                    , classList [ ( "active", link.editor == model.currentEditor ) ]
                    , dataCy link.dataCy
                    , onClick (SetEditor link.editor)
                    ]
                    [ link.icon
                    , span [ attribute "data-content" link.label ] [ text link.label ]
                    ]
                ]

        links =
            [ templateLink
            , filesLink
            , previewLink
            ]
    in
    DetailNavigation.row [ ul [ class "nav nav-underline-tabs" ] (List.map viewLink links) ]
