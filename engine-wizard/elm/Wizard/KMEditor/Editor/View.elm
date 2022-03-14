module Wizard.KMEditor.Editor.View exposing (view)

import ActionResult
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Shared.Html exposing (emptyNode, faSet)
import Shared.Locale exposing (l, lx)
import Shared.Undraw as Undraw
import Uuid
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Components.DetailNavigation as DetailNavigation
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.View.Page as Page
import Wizard.KMEditor.Editor.Common.EditorBranch exposing (EditorBranch)
import Wizard.KMEditor.Editor.Components.KMEditor as KMEditor
import Wizard.KMEditor.Editor.Components.Preview as Preview
import Wizard.KMEditor.Editor.Components.Settings as Settings
import Wizard.KMEditor.Editor.Components.TagEditor as TagEditor
import Wizard.KMEditor.Editor.KMEditorRoute as KMEditorRoute exposing (KMEditorRoute(..))
import Wizard.KMEditor.Editor.Models exposing (Model)
import Wizard.KMEditor.Editor.Msgs exposing (Msg(..))
import Wizard.KMEditor.Routes as KMEditorRoutes exposing (Route(..))
import Wizard.Projects.Detail.Components.PlanSaving as PlanSaving
import Wizard.Routes


l_ : String -> AppState -> String
l_ =
    l "Wizard.KMEditor.Editor.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.KMEditor.Editor.View"


view : KMEditorRoute -> AppState -> Model -> Html Msg
view route appState model =
    if model.error then
        viewError appState

    else if model.offline then
        viewOffline appState

    else
        Page.actionResultView appState (viewKMEditor route appState model) model.branchModel



-- ERROR PAGES


viewOffline : AppState -> Html Msg
viewOffline appState =
    Page.illustratedMessageHtml
        { image = Undraw.warning
        , heading = l_ "offline.heading" appState
        , content =
            [ p [] [ lx_ "offline.text" appState ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ lx_ "offline.refresh" appState ] ]
            ]
        , cy = "offline"
        }


viewError : AppState -> Html Msg
viewError appState =
    Page.illustratedMessageHtml
        { image = Undraw.warning
        , heading = l_ "error.heading" appState
        , content =
            [ p [] [ lx_ "error.text" appState ]
            , p [] [ button [ onClick Refresh, class "btn btn-lg btn-primary" ] [ lx_ "error.refresh" appState ] ]
            ]
        , cy = "error"
        }



-- EDITOR


viewKMEditor : KMEditorRoute -> AppState -> Model -> EditorBranch -> Html Msg
viewKMEditor route appState model branch =
    let
        navigation =
            if AppState.isFullscreen appState then
                emptyNode

            else
                viewKMEditorNavigation appState route model branch
    in
    div [ class "KMEditor__Editor", dataCy "km-editor" ]
        [ navigation
        , viewKMEditorContent appState route model branch
        ]



-- EDITOR - NAVIGATION


viewKMEditorNavigation : AppState -> KMEditorRoute -> Model -> EditorBranch -> Html Msg
viewKMEditorNavigation appState route model branch =
    DetailNavigation.container
        [ viewKMEditorNavigationTitleRow appState model branch
        , viewKMEditorNavigationNav appState route branch
        ]



-- EDITOR - NAVIGATION - TITLE ROW


viewKMEditorNavigationTitleRow : AppState -> Model -> EditorBranch -> Html Msg
viewKMEditorNavigationTitleRow appState model branch =
    let
        publishRoute =
            Wizard.Routes.KMEditorRoute <| PublishRoute branch.branch.uuid
    in
    DetailNavigation.row
        [ DetailNavigation.section
            [ div [ class "title" ] [ text branch.branch.name ]
            , viewKMEditorNavigationSaving appState model
            ]
        , DetailNavigation.section
            [ DetailNavigation.onlineUsers OnlineUserMsg appState model.onlineUsers
            , DetailNavigation.sectionActions
                [ linkTo appState
                    publishRoute
                    [ class "btn btn-primary link-with-icon"
                    , dataCy "km-editor_publish-button"
                    ]
                    [ faSet "kmEditorList.publish" appState
                    , lx_ "header.publish" appState
                    ]
                ]
            ]
        ]


viewKMEditorNavigationSaving : AppState -> Model -> Html Msg
viewKMEditorNavigationSaving appState model =
    Html.map SavingMsg <|
        PlanSaving.view appState model.savingModel



-- EDITOR - NAVIGATION - NAV ROW


viewKMEditorNavigationNav : AppState -> KMEditorRoute -> EditorBranch -> Html Msg
viewKMEditorNavigationNav appState route editorBranch =
    let
        editorRoute subroute =
            Wizard.Routes.KMEditorRoute (KMEditorRoutes.EditorRoute editorBranch.branch.uuid subroute)

        isEditorRoute =
            case route of
                Edit _ ->
                    True

                _ ->
                    False

        editUuid =
            if editorBranch.activeUuid == Uuid.toString editorBranch.branch.knowledgeModel.uuid then
                Nothing

            else
                Just (Uuid.fromUuidString editorBranch.activeUuid)

        editorLink =
            { route = editorRoute (KMEditorRoute.Edit editUuid)
            , label = l_ "nav.knowledgeModel" appState
            , icon = faSet "kmEditor.knowledgeModel" appState
            , isActive = isEditorRoute
            , isVisible = True
            , dataCy = "km-editor_nav_km"
            }

        questionTagsLink =
            { route = editorRoute KMEditorRoute.QuestionTags
            , label = l_ "nav.tags" appState
            , icon = faSet "kmEditor.tags" appState
            , isActive = route == KMEditorRoute.QuestionTags
            , isVisible = True
            , dataCy = "km-editor_nav_tags"
            }

        previewLink =
            { route = editorRoute KMEditorRoute.Preview
            , label = l_ "nav.preview" appState
            , icon = faSet "kmEditor.preview" appState
            , isActive = route == KMEditorRoute.Preview
            , isVisible = True
            , dataCy = "km-editor_nav_preview"
            }

        settingsLink =
            { route = editorRoute KMEditorRoute.Settings
            , label = l_ "nav.settings" appState
            , icon = faSet "kmEditor.settings" appState
            , isActive = route == KMEditorRoute.Settings
            , isVisible = True
            , dataCy = "km-editor_nav_settings"
            }

        links =
            [ editorLink
            , questionTagsLink
            , previewLink
            , settingsLink
            ]
    in
    DetailNavigation.navigation appState links



-- EDITOR - CONTENT


viewKMEditorContent : AppState -> KMEditorRoute -> Model -> EditorBranch -> Html Msg
viewKMEditorContent appState route model editorBranch =
    case route of
        KMEditorRoute.Edit _ ->
            KMEditor.view appState KMEditorMsg EventMsg model.kmEditorModel (ActionResult.withDefault [] model.integrationPrefabs) editorBranch

        KMEditorRoute.QuestionTags ->
            TagEditor.view appState TagEditorMsg EventMsg editorBranch model.tagEditorModel

        KMEditorRoute.Preview ->
            Html.map PreviewMsg <|
                Preview.view appState editorBranch model.previewModel

        KMEditorRoute.Settings ->
            Html.map SettingsMsg <|
                Settings.view appState editorBranch.branch model.settingsModel
