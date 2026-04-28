module Wizard.Components.Questionnaire2.Components.Toolbar exposing
    ( Model
    , Msg
    , UpdateConfig
    , init
    , subscriptions
    , update
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Common.Components.Badge as Badge
import Common.Components.FontAwesome exposing (faQuestionnaireExpand, faQuestionnaireShrink, faSearch, faSuccess, fas)
import Common.Components.Tooltip exposing (tooltip, tooltipRight)
import Common.Utils.KnowledgeModelUtils as KnowledgeModelUtils
import Gettext exposing (gettext)
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Html.Lazy as Lazy
import Uuid exposing (Uuid)
import Wizard.Api.Models.Project.ProjectTodo exposing (ProjectTodo)
import Wizard.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire, QuestionnaireWarning)
import Wizard.Components.Questionnaire2.QuestionnaireRightPanel as QuestionnaireRightPanel exposing (QuestionnaireRightPanel)
import Wizard.Components.Questionnaire2.QuestionnaireViewSettings as QuestionnaireViewSettings exposing (QuestionnaireViewSettings)
import Wizard.Components.Questionnaire2.ToolbarViewFlags as ToolbarViewFlags
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Plugins.Plugin as Plugin exposing (Plugin, ProjectActionConnector, ProjectImporterConnector)
import Wizard.Plugins.PluginElement exposing (PluginElement)
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (linkToAttributes)


type alias Model =
    { uuid : Uuid
    , viewSettingsDropdownState : Dropdown.State
    , pluginActions : List ( Plugin, ProjectActionConnector )
    , pluginActionsDropdownState : Dropdown.State
    , pluginImporters : List ( Plugin, ProjectImporterConnector )
    , pluginImportersDropdownState : Dropdown.State
    }


init : AppState -> ProjectQuestionnaire -> Model
init appState questionnaire =
    let
        pluginActions =
            AppState.getPluginsByConnector appState .projectActions
                |> Plugin.filterByKmPatterns (KnowledgeModelUtils.getPackageId questionnaire.knowledgeModelPackage)
                |> List.sortBy (.name << Tuple.second)

        pluginImporters =
            AppState.getPluginsByConnector appState .projectImporters
                |> Plugin.filterByKmPatterns (KnowledgeModelUtils.getPackageId questionnaire.knowledgeModelPackage)
                |> List.sortBy (.name << Tuple.second)
    in
    { uuid = questionnaire.uuid
    , viewSettingsDropdownState = Dropdown.initialState
    , pluginActions = pluginActions
    , pluginActionsDropdownState = Dropdown.initialState
    , pluginImporters = pluginImporters
    , pluginImportersDropdownState = Dropdown.initialState
    }


type Msg
    = ViewSettingsDropdownMsg Dropdown.State
    | PluginActionsDropdownMsg Dropdown.State
    | PluginImportersDropdownMsg Dropdown.State
    | SetViewSettings QuestionnaireViewSettings
    | OpenPluginProjectActionModal Uuid PluginElement
    | OpenWarnings
    | OpenTodos
    | OpenComments
    | OpenVersionHistory
    | OpenSearch
    | CloseRightPanel
    | SetFullscreen Bool


type alias UpdateConfig msg =
    { updateViewSettingsCmd : QuestionnaireViewSettings -> Cmd msg
    , openProjectActionModalCmd : Uuid -> PluginElement -> Cmd msg
    , updateRightPanelCmd : QuestionnaireRightPanel -> Cmd msg
    , setFullScreenCmd : Bool -> Cmd msg
    }


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update cfg msg model =
    case msg of
        ViewSettingsDropdownMsg dropdownState ->
            ( { model | viewSettingsDropdownState = dropdownState }
            , Cmd.none
            )

        PluginActionsDropdownMsg dropdownState ->
            ( { model | pluginActionsDropdownState = dropdownState }
            , Cmd.none
            )

        PluginImportersDropdownMsg dropdownState ->
            ( { model | pluginImportersDropdownState = dropdownState }
            , Cmd.none
            )

        SetViewSettings newViewSettings ->
            ( model
            , cfg.updateViewSettingsCmd newViewSettings
            )

        OpenPluginProjectActionModal pluginUuid pluginElement ->
            ( model
            , cfg.openProjectActionModalCmd pluginUuid pluginElement
            )

        OpenWarnings ->
            ( model
            , cfg.updateRightPanelCmd QuestionnaireRightPanel.Warnings
            )

        OpenTodos ->
            ( model
            , cfg.updateRightPanelCmd QuestionnaireRightPanel.TODOs
            )

        OpenComments ->
            ( model
            , cfg.updateRightPanelCmd QuestionnaireRightPanel.CommentsOverview
            )

        OpenVersionHistory ->
            ( model
            , cfg.updateRightPanelCmd QuestionnaireRightPanel.VersionHistory
            )

        OpenSearch ->
            ( model
            , cfg.updateRightPanelCmd QuestionnaireRightPanel.Search
            )

        CloseRightPanel ->
            ( model
            , cfg.updateRightPanelCmd QuestionnaireRightPanel.None
            )

        SetFullscreen isFullscreen ->
            ( model
            , cfg.setFullScreenCmd isFullscreen
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Dropdown.subscriptions model.pluginActionsDropdownState PluginActionsDropdownMsg
        , Dropdown.subscriptions model.pluginImportersDropdownState PluginImportersDropdownMsg
        , Dropdown.subscriptions model.viewSettingsDropdownState ViewSettingsDropdownMsg
        ]


view : AppState -> QuestionnaireViewSettings -> QuestionnaireRightPanel -> ProjectQuestionnaire -> List ProjectTodo -> List QuestionnaireWarning -> Model -> Html Msg
view appState viewSettings rightPanel questionnaire todos warnings model =
    let
        toolbarViewFlags =
            ToolbarViewFlags.toInt
                { todosVisible = Feature.projectTodos appState questionnaire
                , commentsVisible = Feature.projectCommentAdd appState questionnaire
                , versionHistoryVisible = Feature.projectVersionHistory appState questionnaire
                , importersVisible = Feature.projectToolbarImporters appState questionnaire
                }
    in
    div [ class "questionnaireToolbar" ]
        [ viewToolbarLeft appState.locale viewSettings model toolbarViewFlags
        , viewToolbarRight appState rightPanel questionnaire todos warnings toolbarViewFlags
        ]


viewToolbarLeft : Gettext.Locale -> QuestionnaireViewSettings -> Model -> Int -> Html Msg
viewToolbarLeft locale viewSettings model toolbarViewFlags =
    Lazy.lazy4 viewToolbarLeftLazy locale viewSettings model toolbarViewFlags


viewToolbarLeftLazy : Gettext.Locale -> QuestionnaireViewSettings -> Model -> Int -> Html Msg
viewToolbarLeftLazy locale viewSettings model toolbarViewFlags =
    let
        viewDropdown =
            let
                settingsIcon enabled =
                    if enabled then
                        faSuccess

                    else
                        Html.nothing

                hiddenOptionsTooltip =
                    if QuestionnaireViewSettings.anyHidden viewSettings then
                        tooltipRight (gettext "Some options are hidden" locale)

                    else
                        []
            in
            div [ class "item-group" ]
                [ Dropdown.dropdown model.viewSettingsDropdownState
                    { options = []
                    , toggleMsg = ViewSettingsDropdownMsg
                    , toggleButton =
                        Dropdown.toggle [ Button.roleLink, Button.attrs [ class "item" ] ]
                            [ div hiddenOptionsTooltip
                                [ text (gettext "View" locale)
                                , Html.viewIf (QuestionnaireViewSettings.anyHidden viewSettings) <|
                                    span [ class "ms-2 text-danger" ]
                                        [ fas "fa-circle fa-2xs" ]
                                ]
                            ]
                    , items =
                        [ Dropdown.anchorItem
                            [ onClick (SetViewSettings QuestionnaireViewSettings.all) ]
                            [ text (gettext "Show all" locale) ]
                        , Dropdown.anchorItem
                            [ onClick (SetViewSettings QuestionnaireViewSettings.none) ]
                            [ text (gettext "Hide all" locale) ]
                        , Dropdown.divider
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon", onClick (SetViewSettings (QuestionnaireViewSettings.toggleAnsweredBy viewSettings)) ]
                            [ settingsIcon viewSettings.answeredBy, text (gettext "Answered by" locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.togglePhases viewSettings))
                            ]
                            [ settingsIcon viewSettings.phases, text (gettext "Phases" locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.toggleTags viewSettings))
                            ]
                            [ settingsIcon viewSettings.tags, text (gettext "Question tags" locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.toggleNonDesirableQuestions viewSettings))
                            ]
                            [ settingsIcon viewSettings.nonDesirableQuestions, text (gettext "Non-desirable questions" locale) ]
                        , Dropdown.anchorItem
                            [ class "dropdown-item-icon"
                            , onClick (SetViewSettings (QuestionnaireViewSettings.toggleMetricValues viewSettings))
                            ]
                            [ settingsIcon viewSettings.metricValues, text (gettext "Metric values" locale) ]
                        ]
                    }
                ]

        pluginDropdownItem ( _, connector ) =
            Dropdown.anchorItem
                (class "dropdown-item" :: linkToAttributes (Routes.projectsImport model.uuid connector.url))
                [ text connector.name ]

        pluginImportersDropdown =
            Html.viewIf (ToolbarViewFlags.importersVisible toolbarViewFlags && not (List.isEmpty model.pluginImporters)) <|
                div [ class "item-group" ]
                    [ Dropdown.dropdown model.pluginImportersDropdownState
                        { options = []
                        , toggleMsg = PluginImportersDropdownMsg
                        , toggleButton =
                            Dropdown.toggle [ Button.roleLink, Button.attrs [ class "item" ] ]
                                [ text (gettext "Import" locale) ]
                        , items = List.map pluginDropdownItem model.pluginImporters
                        }
                    ]

        pluginActionItem ( plugin, connector ) =
            Dropdown.anchorItem
                [ class "dropdown-item"
                , onClick (OpenPluginProjectActionModal plugin.uuid connector.element)
                ]
                [ text connector.name ]

        pluginActionsDropdown =
            Html.viewIf (not (List.isEmpty model.pluginActions)) <|
                div [ class "item-group" ]
                    [ Dropdown.dropdown model.pluginActionsDropdownState
                        { options = []
                        , toggleMsg = PluginActionsDropdownMsg
                        , toggleButton =
                            Dropdown.toggle [ Button.roleLink, Button.attrs [ class "item" ] ]
                                [ span [ class "icon" ] []
                                , text (gettext "Actions" locale)
                                ]
                        , items = List.map pluginActionItem model.pluginActions
                        }
                    ]
    in
    div [ class "questionnaireToolbar__left" ]
        [ viewDropdown
        , pluginImportersDropdown
        , pluginActionsDropdown
        ]


viewToolbarRight : AppState -> QuestionnaireRightPanel -> ProjectQuestionnaire -> List ProjectTodo -> List QuestionnaireWarning -> Int -> Html Msg
viewToolbarRight appState rightPanel questionnaire todos warnings toolbarViewFlags =
    let
        warningsLength =
            List.length warnings

        todosLength =
            List.length todos

        commentsCount =
            ProjectQuestionnaire.commentsLength questionnaire

        isFullScreen =
            AppState.isFullscreen appState
    in
    Lazy.lazy7 viewToolbarRightLazy
        appState.locale
        rightPanel
        warningsLength
        todosLength
        commentsCount
        isFullScreen
        toolbarViewFlags


viewToolbarRightLazy : Gettext.Locale -> QuestionnaireRightPanel -> Int -> Int -> Int -> Bool -> Int -> Html Msg
viewToolbarRightLazy locale rightPanel warningsLength todosLength commentsCount isFullScreen toolbarViewFlags =
    div [ class "questionnaireToolbar__right" ]
        [ warningsButton locale rightPanel warningsLength
        , todosButton locale rightPanel (ToolbarViewFlags.todosVisible toolbarViewFlags) todosLength
        , commentsButton locale rightPanel (ToolbarViewFlags.commentsVisible toolbarViewFlags) commentsCount
        , versionHistoryButton locale rightPanel (ToolbarViewFlags.versionHistoryVisible toolbarViewFlags)
        , searchButton locale rightPanel
        , fullScreenButton isFullScreen
        ]


warningsButton : Gettext.Locale -> QuestionnaireRightPanel -> Int -> Html Msg
warningsButton locale rightPanel warningsLength =
    let
        warningsOpen =
            rightPanel == QuestionnaireRightPanel.Warnings

        onClickAction =
            if warningsOpen then
                CloseRightPanel

            else
                OpenWarnings
    in
    Html.viewIf (warningsLength > 0 || warningsOpen) <|
        div [ class "item-group" ]
            [ a
                [ class "item"
                , classList [ ( "selected", warningsOpen ) ]
                , onClick onClickAction
                ]
                [ text (gettext "Warnings" locale)
                , Html.viewIf (warningsLength > 0) <|
                    Badge.danger [ class "rounded-pill" ] [ text (String.fromInt warningsLength) ]
                ]
            ]


todosButton : Gettext.Locale -> QuestionnaireRightPanel -> Bool -> Int -> Html Msg
todosButton locale rightPanel todosVisible todosLength =
    let
        todosOpen =
            rightPanel == QuestionnaireRightPanel.TODOs

        onClickAction =
            if todosOpen then
                CloseRightPanel

            else
                OpenTodos
    in
    Html.viewIf (todosVisible || todosOpen) <|
        div [ class "item-group" ]
            [ a
                [ class "item"
                , classList [ ( "selected", todosOpen ) ]
                , onClick onClickAction
                ]
                [ text (gettext "TODOs" locale)
                , Html.viewIf (todosLength > 0) <|
                    Badge.danger [ class "rounded-pill" ] [ text (String.fromInt todosLength) ]
                ]
            ]


commentsButton : Gettext.Locale -> QuestionnaireRightPanel -> Bool -> Int -> Html Msg
commentsButton locale rightPanel commentsVisible unresolvedCommentsCount =
    let
        commentsOpen =
            case rightPanel of
                QuestionnaireRightPanel.CommentsOverview ->
                    True

                QuestionnaireRightPanel.Comments _ ->
                    True

                _ ->
                    False

        onClickAction =
            if rightPanel == QuestionnaireRightPanel.CommentsOverview then
                CloseRightPanel

            else
                OpenComments
    in
    Html.viewIf commentsVisible <|
        div [ class "item-group" ]
            [ a
                [ class "item"
                , classList [ ( "selected", commentsOpen ) ]
                , onClick onClickAction
                ]
                [ text (gettext "Comments" locale)
                , Html.viewIf (unresolvedCommentsCount > 0) <|
                    Badge.secondary
                        [ class "rounded-pill"
                        , dataCy "questionnaire_toolbar_comments_count"
                        ]
                        [ text (String.fromInt unresolvedCommentsCount) ]
                ]
            ]


versionHistoryButton : Gettext.Locale -> QuestionnaireRightPanel -> Bool -> Html Msg
versionHistoryButton locale rightPanel versionHistoryVisible =
    let
        versionHistoryOpen =
            rightPanel == QuestionnaireRightPanel.VersionHistory

        onClickAction =
            if versionHistoryOpen then
                CloseRightPanel

            else
                OpenVersionHistory
    in
    Html.viewIf versionHistoryVisible <|
        div [ class "item-group" ]
            [ a
                [ class "item"
                , classList [ ( "selected", versionHistoryOpen ) ]
                , onClick onClickAction
                , dataCy "questionnaire-version-history"
                ]
                [ text (gettext "Version history" locale) ]
            ]


searchButton : Gettext.Locale -> QuestionnaireRightPanel -> Html Msg
searchButton locale rightPanel =
    let
        searchOpen =
            rightPanel == QuestionnaireRightPanel.Search

        onClickAction =
            if searchOpen then
                CloseRightPanel

            else
                OpenSearch
    in
    div [ class "item-group" ]
        [ a
            (tooltip (gettext "Search" locale)
                ++ [ class "item"
                   , classList [ ( "selected", searchOpen ) ]
                   , onClick onClickAction
                   , dataCy "questionnaire-search"
                   ]
            )
            [ faSearch ]
        ]


fullScreenButton : Bool -> Html Msg
fullScreenButton isFullscreen =
    let
        ( expandIcon, expandMsg ) =
            if isFullscreen then
                ( faQuestionnaireShrink, SetFullscreen False )

            else
                ( faQuestionnaireExpand, SetFullscreen True )
    in
    div [ class "item-group" ]
        [ a [ class "item", onClick expandMsg ] [ expandIcon ]
        ]
