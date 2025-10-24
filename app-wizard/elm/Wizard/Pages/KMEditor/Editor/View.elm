module Wizard.Pages.KMEditor.Editor.View exposing (view)

import ActionResult
import Common.Components.FontAwesome exposing (faKmEditorKnowledgeModel, faKmEditorListPublish, faKmEditorTags, faKmPhase, faPreview, faSettings)
import Common.Components.Page as Page
import Common.Components.Undraw as Undraw
import Gettext exposing (gettext)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Uuid
import Wizard.Components.DetailNavigation as DetailNavigation
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.KMEditor.Editor.Common.EditorContext exposing (EditorContext)
import Wizard.Pages.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.Pages.KMEditor.Editor.Components.PhaseEditor as PhaseEditor
import Wizard.Pages.KMEditor.Editor.Components.Preview as Preview
import Wizard.Pages.KMEditor.Editor.Components.PublishModal as PublishModal
import Wizard.Pages.KMEditor.Editor.Components.Settings as Settings
import Wizard.Pages.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.Pages.KMEditor.Editor.KMEditorRoute as KMEditorRoute exposing (KMEditorRoute(..))
import Wizard.Pages.KMEditor.Editor.Models exposing (Model)
import Wizard.Pages.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.Pages.Projects.Detail.Components.ProjectSaving as ProjectSaving
import Wizard.Routes as Routes


view : KMEditorRoute -> AppState -> Model -> Html Msg
view route appState model =
    if model.error then
        viewError appState

    else if model.offline then
        viewOffline appState

    else
        Page.actionResultView appState (viewKMEditor route appState model) model.editorContext



-- ERROR PAGES


viewOffline : AppState -> Html Msg
viewOffline appState =
    Page.illustratedMessageHtml
        { illustration = Undraw.warning
        , heading = gettext "Disconnected" appState.locale
        , content =
            [ p [] [ text (gettext "You have been disconnected, try to refresh the page." appState.locale) ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ text (gettext "Refresh" appState.locale) ] ]
            ]
        , cy = "offline"
        }


viewError : AppState -> Html Msg
viewError appState =
    Page.illustratedMessageHtml
        { illustration = Undraw.warning
        , heading = gettext "Oops!" appState.locale
        , content =
            [ p [] [ text (gettext "Something went wrong, try to refresh the page." appState.locale) ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ text (gettext "Refresh" appState.locale) ] ]
            ]
        , cy = "error"
        }



-- EDITOR


viewKMEditor : KMEditorRoute -> AppState -> Model -> EditorContext -> Html Msg
viewKMEditor route appState model editorContext =
    let
        navigation =
            if AppState.isFullscreen appState then
                Html.nothing

            else
                viewKMEditorNavigation appState route model editorContext

        publishModalViewConfig =
            { kmEditor = editorContext.kmEditor }
    in
    div [ class "KMEditor__Editor col-full flex-column", dataCy "km-editor" ]
        [ navigation
        , viewKMEditorContent appState route model editorContext
        , Html.map PublishModalMsg <| PublishModal.view publishModalViewConfig appState model.publishModalModel
        ]



-- EDITOR - NAVIGATION


viewKMEditorNavigation : AppState -> KMEditorRoute -> Model -> EditorContext -> Html Msg
viewKMEditorNavigation appState route model editorContext =
    DetailNavigation.container
        [ viewKMEditorNavigationTitleRow appState model editorContext
        , viewKMEditorNavigationNav appState route editorContext
        ]



-- EDITOR - NAVIGATION - TITLE ROW


viewKMEditorNavigationTitleRow : AppState -> Model -> EditorContext -> Html Msg
viewKMEditorNavigationTitleRow appState model editorContext =
    DetailNavigation.row
        [ DetailNavigation.section
            [ div [ class "title" ] [ text editorContext.kmEditor.name ]
            , viewKMEditorNavigationSaving appState model
            ]
        , DetailNavigation.section
            [ DetailNavigation.onlineUsers appState False model.onlineUsers
            , DetailNavigation.sectionActions
                [ button
                    [ class "btn btn-primary with-icon"
                    , onClick (PublishModalMsg PublishModal.openMsg)
                    , dataCy "km-editor_publish-button"
                    ]
                    [ faKmEditorListPublish
                    , text (gettext "Publish" appState.locale)
                    ]
                ]
            ]
        ]


viewKMEditorNavigationSaving : AppState -> Model -> Html Msg
viewKMEditorNavigationSaving appState model =
    Html.map SavingMsg <|
        ProjectSaving.view appState model.savingModel



-- EDITOR - NAVIGATION - NAV ROW


viewKMEditorNavigationNav : AppState -> KMEditorRoute -> EditorContext -> Html Msg
viewKMEditorNavigationNav appState route editorContext =
    let
        kmEditorUuid =
            editorContext.kmEditor.uuid

        isEditorRoute =
            case route of
                Edit _ ->
                    True

                _ ->
                    False

        editUuid =
            if editorContext.activeUuid == Uuid.toString editorContext.kmEditor.knowledgeModel.uuid then
                Nothing

            else
                Just (Uuid.fromUuidString editorContext.activeUuid)

        editorLink =
            { route = Routes.kmEditorEditor kmEditorUuid editUuid
            , label = gettext "Knowledge Model" appState.locale
            , icon = faKmEditorKnowledgeModel
            , isActive = isEditorRoute
            , isVisible = True
            , dataCy = "km-editor_nav_km"
            }

        phasesLink =
            { route = Routes.kmEditorEditorPhases kmEditorUuid
            , label = gettext "Phases" appState.locale
            , icon = faKmPhase
            , isActive = route == KMEditorRoute.Phases
            , isVisible = True
            , dataCy = "km-editor_nav_phases"
            }

        questionTagsLink =
            { route = Routes.kmEditorEditorQuestionTags kmEditorUuid
            , label = gettext "Question Tags" appState.locale
            , icon = faKmEditorTags
            , isActive = route == KMEditorRoute.QuestionTags
            , isVisible = True
            , dataCy = "km-editor_nav_tags"
            }

        previewLink =
            { route = Routes.kmEditorEditorPreview kmEditorUuid
            , label = gettext "Preview" appState.locale
            , icon = faPreview
            , isActive = route == KMEditorRoute.Preview
            , isVisible = True
            , dataCy = "km-editor_nav_preview"
            }

        settingsLink =
            { route = Routes.kmEditorEditorSettings kmEditorUuid
            , label = gettext "Settings" appState.locale
            , icon = faSettings
            , isActive = route == KMEditorRoute.Settings
            , isVisible = True
            , dataCy = "km-editor_nav_settings"
            }

        links =
            [ editorLink
            , phasesLink
            , questionTagsLink
            , previewLink
            , settingsLink
            ]
    in
    DetailNavigation.navigation links



-- EDITOR - CONTENT


viewKMEditorContent : AppState -> KMEditorRoute -> Model -> EditorContext -> Html Msg
viewKMEditorContent appState route model editorContext =
    case route of
        KMEditorRoute.Edit _ ->
            KMEditor.view appState KMEditorMsg EventMsg model.kmEditorModel (ActionResult.withDefault [] model.integrationPrefabs) (ActionResult.withDefault [] model.kmSecrets) editorContext

        KMEditorRoute.Phases ->
            PhaseEditor.view appState PhaseEditorMsg (EventMsg False Nothing Nothing) editorContext model.phaseEditorModel

        KMEditorRoute.QuestionTags ->
            TagEditor.view appState TagEditorMsg (EventMsg False Nothing Nothing) editorContext model.tagEditorModel

        KMEditorRoute.Preview ->
            let
                previewViewConfig =
                    { editorContext = editorContext
                    , wrapMsg = PreviewMsg
                    , saveRepliesMsg = SavePreviewReplies
                    }
            in
            Preview.view appState previewViewConfig model.previewModel

        KMEditorRoute.Settings ->
            Html.map SettingsMsg <|
                Settings.view appState editorContext.kmEditor model.settingsModel
