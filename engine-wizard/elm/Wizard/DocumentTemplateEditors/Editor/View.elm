module Wizard.DocumentTemplateEditors.Editor.View exposing (view)

import ActionResult
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
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
import Wizard.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute exposing (DTEditorRoute)
import Wizard.DocumentTemplateEditors.Editor.Models exposing (CurrentEditor(..), Model, containsChanges)
import Wizard.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))
import Wizard.Routes as Routes


view : AppState -> DTEditorRoute -> Model -> Html Msg
view appState route model =
    Page.actionResultView appState (viewTemplateEditor appState route model) model.documentTemplate


viewTemplateEditor : AppState -> DTEditorRoute -> Model -> DocumentTemplateDraftDetail -> Html Msg
viewTemplateEditor appState route model documentTemplate =
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
        [ viewEditorNavigation appState route model
        , content
        , Html.map PublishModalMsg <| PublishModal.view publishModalViewConfig appState model.publishModalModel
        ]


viewEditorNavigation : AppState -> DTEditorRoute -> Model -> Html Msg
viewEditorNavigation appState route model =
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

                    discardButton =
                        button
                            [ class "btn btn-outline-secondary btn-with-loader ms-1"
                            , onClick DiscardChanges
                            ]
                            [ text (gettext "Discard" appState.locale) ]
                in
                [ span [ class "me-2" ]
                    [ text "("
                    , text (gettext "unsaved changes" appState.locale)
                    , text ")"
                    ]
                , saveButton
                , discardButton
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
        , viewEditorNavigationNav appState route model
        ]


viewEditorNavigationNav : AppState -> DTEditorRoute -> Model -> Html Msg
viewEditorNavigationNav appState route model =
    let
        templateLink =
            { route = Routes.documentTemplateEditorDetail model.documentTemplateId
            , label = gettext "Template" appState.locale
            , icon = faSet "documentTemplateEditor.template" appState
            , isActive = route == DTEditorRoute.Template
            , isVisible = True
            , dataCy = "dt-editor_nav_template"
            }

        filesLink =
            { route = Routes.documentTemplateEditorDetailFiles model.documentTemplateId
            , label = gettext "Files" appState.locale
            , icon = faSet "documentTemplateEditor.files" appState
            , isActive = route == DTEditorRoute.Files
            , isVisible = True
            , dataCy = "dt-editor_nav_files"
            }

        previewLink =
            { route = Routes.documentTemplateEditorDetailPreview model.documentTemplateId
            , label = gettext "Preview" appState.locale
            , icon = faSet "_global.preview" appState
            , isActive = route == DTEditorRoute.Preview
            , isVisible = True
            , dataCy = "dt-editor_nav_preview"
            }

        links =
            [ templateLink
            , filesLink
            , previewLink
            ]
    in
    DetailNavigation.navigation appState links
