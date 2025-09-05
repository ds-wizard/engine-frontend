module Wizard.Pages.DocumentTemplateEditors.Editor.View exposing (view)

import ActionResult
import Common.Components.ActionButton as ActionResult
import Common.Components.FontAwesome exposing (faDocumentTemplateEditorFiles, faDocumentTemplateEditorPublish, faPreview, faSettings)
import Common.Components.Page as Page
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Wizard.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Wizard.Components.DetailNavigation as DetailNavigation
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.FileEditor as FileEditor
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.Preview as Preview
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.PublishModal as PublishModal
import Wizard.Pages.DocumentTemplateEditors.Editor.Components.Settings as Settings
import Wizard.Pages.DocumentTemplateEditors.Editor.DTEditorRoute as DTEditorRoute exposing (DTEditorRoute)
import Wizard.Pages.DocumentTemplateEditors.Editor.Models exposing (CurrentEditor(..), Model, containsChanges)
import Wizard.Pages.DocumentTemplateEditors.Editor.Msgs exposing (Msg(..))
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
                    let
                        viewConfig =
                            { documentTemplateFormatPrefabs = model.documentTemplateFormatPrefabs
                            , documentTemplateFormatStepPrefabs = model.documentTemplateFormatStepPrefabs
                            }
                    in
                    Html.map SettingsMsg <|
                        Settings.view appState viewConfig model.settingsModel

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
                        ActionResult.buttonWithAttrs
                            { label = gettext "Save" appState.locale
                            , result =
                                ActionResult.all
                                    (ActionResult.map (always ()) model.settingsModel.savingForm
                                        :: Dict.values model.fileEditorModel.savingFiles
                                    )
                            , msg = Save
                            , dangerous = False
                            , attrs = [ dataCy "dt-editor_save" ]
                            }

                    discardButton =
                        button
                            [ class "btn btn-outline-secondary btn-wide ms-1"
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
                    [ faDocumentTemplateEditorPublish
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
        filesLink =
            { route = Routes.documentTemplateEditorDetailFiles model.documentTemplateId
            , label = gettext "Files" appState.locale
            , icon = faDocumentTemplateEditorFiles
            , isActive = route == DTEditorRoute.Files
            , isVisible = True
            , dataCy = "dt-editor_nav_files"
            }

        previewLink =
            { route = Routes.documentTemplateEditorDetailPreview model.documentTemplateId
            , label = gettext "Preview" appState.locale
            , icon = faPreview
            , isActive = route == DTEditorRoute.Preview
            , isVisible = True
            , dataCy = "dt-editor_nav_preview"
            }

        settingsLink =
            { route = Routes.documentTemplateEditorDetailSettings model.documentTemplateId
            , label = gettext "Settings" appState.locale
            , icon = faSettings
            , isActive = route == DTEditorRoute.Settings
            , isVisible = True
            , dataCy = "dt-editor_nav_settings"
            }

        links =
            [ filesLink
            , previewLink
            , settingsLink
            ]
    in
    DetailNavigation.navigation links
