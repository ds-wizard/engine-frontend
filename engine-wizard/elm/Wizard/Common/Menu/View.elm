module Wizard.Common.Menu.View exposing (view, viewAboutModal, viewReportIssueModal)

import ActionResult exposing (ActionResult)
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, button, code, div, em, h5, img, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, colspan, href, id, src, style, target)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Json.Decode as D
import Json.Decode.Extra as D
import Shared.Auth.Role as Role
import Shared.Common.TimeUtils as TimeUtils
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.BootstrapConfig.AppSwitcherItem as AppSwitcherItem exposing (AppSwitcherItem)
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink exposing (CustomMenuLink)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, fa, faSet, faSetFw)
import String.Format as String
import Wizard.Auth.Msgs
import Wizard.Common.AppState as AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltip)
import Wizard.Common.Html.Events exposing (onLinkClick)
import Wizard.Common.Menu.Msgs exposing (Msg(..))
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Models exposing (Model)
import Wizard.Msgs
import Wizard.Routes as Routes exposing (Route)
import Wizard.Routing as Routing


type MenuItem
    = MenuGroup MenuGroupData
    | MenuItem MenuItemData


type alias MenuGroupData =
    { title : String
    , icon : Html Wizard.Msgs.Msg
    , id : String
    , route : Route
    , isActive : Route -> Bool
    , isVisible : AppState -> Bool
    , items : List GroupItemData
    }


type alias MenuItemData =
    { title : String
    , icon : Html Wizard.Msgs.Msg
    , id : String
    , route : Route
    , isActive : Route -> Bool
    , isVisible : AppState -> Bool
    }


type alias GroupItemData =
    { title : String
    , id : String
    , route : Route
    , isActive : Route -> Bool
    , isVisible : AppState -> Bool
    }


menuItems : AppState -> List MenuItem
menuItems appState =
    [ MenuItem
        { title = gettext "Dashboard" appState.locale
        , icon = faSetFw "menu.dashboard" appState
        , id = "dashboard"
        , route = Routes.dashboard
        , isActive = Routes.isDashboard
        , isVisible = always True
        }
    , MenuItem
        { title = "Tenants"
        , icon = faSetFw "menu.tenants" appState
        , id = "tenants"
        , route = Routes.tenantsIndex
        , isActive = Routes.isTenantIndex
        , isVisible = Feature.tenants
        }
    , MenuGroup
        { title = gettext "Knowledge Models" appState.locale
        , icon = faSetFw "menu.knowledgeModels" appState
        , id = "knowledge-models"
        , route = Routes.knowledgeModelsIndex
        , isActive = Routes.isKnowledgeModelsSubroute
        , isVisible = Feature.knowledgeModelsImport
        , items =
            [ { title = gettext "List" appState.locale
              , id = "knowledge-models-list"
              , route = Routes.knowledgeModelsIndex
              , isActive = Routes.isKnowledgeModelsIndex
              , isVisible = always True
              }
            , { title = gettext "Editors" appState.locale
              , id = "knowledge-models-editors"
              , route = Routes.kmEditorIndex
              , isActive = Routes.isKmEditorIndex
              , isVisible = always True
              }
            ]
        }
    , MenuGroup
        { title = gettext "Document Templates" appState.locale
        , icon = faSetFw "menu.templates" appState
        , id = "document-templates"
        , route = Routes.documentTemplatesIndex
        , isActive = Routes.isDocumentTemplatesSubroute
        , isVisible = Feature.documentTemplatesView
        , items =
            [ { title = gettext "List" appState.locale
              , id = "documents-list"
              , route = Routes.documentTemplatesIndex
              , isActive = Routes.isDocumentTemplatesIndex
              , isVisible = always True
              }
            , { title = gettext "Editors" appState.locale
              , id = "document-editors"
              , route = Routes.documentTemplateEditorsIndex
              , isActive = Routes.isDocumentTemplateEditorsIndex
              , isVisible = always True
              }
            ]
        }
    , MenuItem
        { title = gettext "Projects" appState.locale
        , icon = faSetFw "menu.projects" appState
        , id = "projects"
        , route = Routes.projectsIndex appState
        , isActive = Routes.isProjectsIndex
        , isVisible = \a -> not (Feature.isDataSteward a || Feature.isAdmin a)
        }
    , MenuGroup
        { title = gettext "Projects" appState.locale
        , icon = faSetFw "menu.projects" appState
        , id = "projects"
        , route = Routes.projectsIndex appState
        , isActive = Routes.isProjectSubroute
        , isVisible = \a -> Feature.isDataSteward a || Feature.isAdmin a
        , items =
            [ { title = gettext "List" appState.locale
              , id = "projects-list"
              , route = Routes.projectsIndex appState
              , isActive = Routes.isProjectsIndex
              , isVisible = Feature.projectsView
              }
            , { title = gettext "Files" appState.locale
              , id = "projects-files"
              , route = Routes.projectFilesIndex
              , isActive = Routes.isProjectFilesIndex
              , isVisible = Feature.projectFiles
              }
            , { title = gettext "Documents" appState.locale
              , id = "documents"
              , route = Routes.documentsIndex
              , isActive = Routes.isDocumentsIndex
              , isVisible = Feature.documentsView
              }
            , { title = gettext "Actions" appState.locale
              , id = "projects-actions"
              , route = Routes.projectActionsIndex
              , isActive = Routes.isProjectActionsIndex
              , isVisible = always False
              }
            , { title = gettext "Importers" appState.locale
              , id = "projects-importers"
              , route = Routes.projectImportersIndex
              , isActive = Routes.isProjectImportersIndex
              , isVisible = Feature.projectImporters
              }
            ]
        }
    , MenuGroup
        { title = "Dev"
        , icon = faSetFw "menu.dev" appState
        , id = "dev"
        , route = Routes.devOperations
        , isActive = Routes.isDevSubroute
        , isVisible = Feature.dev
        , items =
            [ { title = "Operations"
              , id = "dev-operations"
              , route = Routes.devOperations
              , isActive = Routes.isDevOperations
              , isVisible = always True
              }
            , { title = "Persistent Commands"
              , id = "dev-persistent-commands"
              , route = Routes.persistentCommandsIndex
              , isActive = Routes.isPersistentCommandsIndex
              , isVisible = always True
              }
            ]
        }
    , MenuItem
        { title = gettext "Settings" appState.locale
        , icon = faSetFw "menu.administration" appState
        , id = "settings"
        , route = Routes.settingsDefault (Admin.isEnabled appState.config.admin)
        , isActive = Routes.isSettingsRoute
        , isVisible = \a -> Admin.isEnabled a.config.admin && Feature.isAdmin a
        }
    , MenuGroup
        { title = gettext "Administration" appState.locale
        , icon = faSetFw "menu.administration" appState
        , id = "administration"
        , route = Routes.settingsDefault (Admin.isEnabled appState.config.admin)
        , isActive = Routes.isSettingsSubroute
        , isVisible = \a -> not (Admin.isEnabled a.config.admin) && Feature.settings a
        , items =
            { title = gettext "Settings" appState.locale
            , id = "system-settings"
            , route = Routes.settingsDefault (Admin.isEnabled appState.config.admin)
            , isActive = Routes.isSettingsRoute
            , isVisible = always True
            }
                :: (if Admin.isEnabled appState.config.admin then
                        []

                    else
                        [ { title = gettext "Users" appState.locale
                          , id = "users"
                          , route = Routes.usersIndex
                          , isActive = Routes.isUsersIndex
                          , isVisible = always True
                          }
                        ]
                   )
                ++ [ { title = gettext "Locales" appState.locale
                     , id = "system-locales"
                     , route = Routes.localesIndex
                     , isActive = Routes.isLocalesRoute
                     , isVisible = always True
                     }
                   ]
        }
    ]


view : Model -> Html Wizard.Msgs.Msg
view model =
    let
        viewAiAssistant =
            if model.appState.config.aiAssistant.enabled then
                div []
                    [ a
                        [ class "sidebar-ai-assistant"
                        , onClick (Wizard.Msgs.SetRightPanelCollapsed (not model.appState.session.rightPanelCollapsed))
                        ]
                        [ fa "fas fa-fw fa-wand-magic-sparkles"
                        , span [ class "sidebar-link" ] [ text "AI Assistant" ]
                        ]
                    ]

            else
                emptyNode
    in
    div [ class "side-navigation", classList [ ( "side-navigation-collapsed", model.appState.session.sidebarCollapsed ) ] ]
        [ viewLogo model
        , viewMenu model
        , viewSessionWarning model
        , viewSessionWarningCollapsed model
        , viewAiAssistant
        , viewProfileMenu model
        , viewCollapseLink model
        ]


viewLogo : Model -> Html Wizard.Msgs.Msg
viewLogo model =
    let
        logoText =
            span [ class "logo-full", dataCy "nav_app-title-short" ]
                [ text <| LookAndFeelConfig.getAppTitleShort model.appState.config.lookAndFeel
                ]
    in
    if List.isEmpty model.appState.config.modules then
        linkTo model.appState
            Routes.appHome
            [ class "logo" ]
            [ img [ class "logo-img", src (LookAndFeelConfig.getLogoUrl model.appState.config.lookAndFeel) ] []
            , logoText
            ]

    else
        let
            itemId =
                "app_switcher"

            mouseenter =
                onMouseEnter (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.GetElement itemId))

            mouseleave =
                onMouseLeave (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.HideElement itemId))

            ( submenuStyle, submenuClass ) =
                case Dict.get itemId model.menuModel.submenuPositions of
                    Just element ->
                        let
                            top =
                                element.element.y - element.viewport.y
                        in
                        ( [ style "top" (String.fromFloat top ++ "px") ], "show" )

                    _ ->
                        ( [], "" )

            viewMenuItem : AppSwitcherItem -> Html Wizard.Msgs.Msg
            viewMenuItem item =
                let
                    icon =
                        case item.icon of
                            AppSwitcherItem.ImageAppSwitcherItemIcon imageSrc ->
                                img [ src imageSrc ] []

                            AppSwitcherItem.FontAwesomeAppSwitcherItemIcon faIcon ->
                                fa faIcon
                in
                li []
                    [ a
                        [ href item.url
                        , classList
                            [ ( "internal", not item.external )
                            , ( "external", item.external )
                            ]
                        ]
                        [ span [ class "icon" ]
                            [ icon
                            ]
                        , span [ class "content" ]
                            [ span [ class "title" ] [ text (gettext item.title model.appState.locale) ]
                            , span [ class "description" ] [ text (gettext item.description model.appState.locale) ]
                            ]
                        ]
                    ]

            internalItems =
                model.appState.config.modules
                    |> List.filter (not << .external)
                    |> List.map viewMenuItem

            externalItems =
                model.appState.config.modules
                    |> List.filter .external
                    |> List.map viewMenuItem

            switchToHeading =
                if List.isEmpty internalItems then
                    emptyNode

                else
                    li [ class "heading" ] [ text (gettext "Switch to" model.appState.locale) ]

            moreHeading =
                if not (List.isEmpty internalItems) && not (List.isEmpty externalItems) then
                    li [ class "heading-2" ] [ text (gettext "More" model.appState.locale) ]

                else
                    emptyNode
        in
        div [ id itemId, class "logo logo-app-switcher", mouseenter, mouseleave ]
            [ img [ class "logo-img", src (LookAndFeelConfig.getLogoUrl model.appState.config.lookAndFeel) ] []
            , logoText
            , span [ class "switcher-icon ms-1 me-2 d-flex text-secondary" ]
                [ fa "fas fa-ellipsis-v"
                , fa "fas fa-ellipsis-v"
                , fa "fas fa-ellipsis-v"
                ]
            , div ([ class "app-switcher-menu", class submenuClass ] ++ submenuStyle)
                [ ul []
                    (switchToHeading
                        :: internalItems
                        ++ [ moreHeading ]
                        ++ externalItems
                    )
                ]
            ]


viewMenu : Model -> Html Wizard.Msgs.Msg
viewMenu model =
    let
        menuHeading =
            if Admin.isEnabled model.appState.config.admin then
                li [ class "heading" ] [ text (gettext LookAndFeelConfig.defaultMenuTitle model.appState.locale) ]

            else
                emptyNode

        filterMenuItem menuItem =
            case menuItem of
                MenuGroup group ->
                    group.isVisible model.appState

                MenuItem item ->
                    item.isVisible model.appState

        defaultMenuItems =
            menuItems model.appState
                |> List.filter filterMenuItem
                |> List.map (defaultMenuItem model)

        customMenuItems =
            List.indexedMap (customMenuItem model) model.appState.config.lookAndFeel.customMenuLinks

        space =
            if List.isEmpty customMenuItems then
                emptyNode

            else
                li [ class "empty" ] []
    in
    ul [ class "menu" ]
        (menuHeading :: defaultMenuItems ++ [ space ] ++ customMenuItems)


defaultMenuItem : Model -> MenuItem -> Html Wizard.Msgs.Msg
defaultMenuItem model item =
    case item of
        MenuGroup menuGroup ->
            let
                viewGroupItem groupItem =
                    if groupItem.isVisible model.appState then
                        Just <|
                            li [ classList [ ( "active", groupItem.isActive model.appState.route ) ] ]
                                [ linkTo model.appState
                                    groupItem.route
                                    []
                                    [ text groupItem.title ]
                                ]

                    else
                        Nothing

                submenuClass =
                    if not model.appState.session.sidebarCollapsed && menuGroup.isActive model.appState.route then
                        "submenu-group"

                    else
                        "submenu-floating submenu-floating-group"

                menuItemId =
                    "menu_" ++ menuGroup.id

                mouseenter =
                    onMouseEnter (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.GetElement menuItemId))

                mouseleave =
                    onMouseLeave (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.HideElement menuItemId))

                ( submenuStyle, submenuExtraClass ) =
                    case Dict.get menuItemId model.menuModel.submenuPositions of
                        Just element ->
                            ( [ style "top" (String.fromFloat (element.element.y - element.viewport.y) ++ "px") ], "show" )

                        _ ->
                            ( [], "" )

                submenuHeading =
                    if model.appState.session.sidebarCollapsed then
                        li [ class "submenu-heading" ] [ text menuGroup.title ]

                    else
                        emptyNode
            in
            li [ id menuItemId, classList [ ( "active", menuGroup.isActive model.appState.route ) ], mouseenter, mouseleave ]
                [ linkTo model.appState
                    menuGroup.route
                    []
                    [ menuGroup.icon
                    , span [ class "sidebar-link" ] [ text menuGroup.title ]
                    ]
                , div ([ class "submenu", class submenuClass, class submenuExtraClass ] ++ submenuStyle)
                    [ ul [] (submenuHeading :: List.filterMap viewGroupItem menuGroup.items) ]
                ]

        MenuItem menuItem ->
            menuLinkSimple model
                (linkTo model.appState menuItem.route [ dataCy ("menu_" ++ menuItem.id) ])
                menuItem.id
                menuItem.icon
                menuItem.title
                (menuItem.isActive model.appState.route)


customMenuItem : Model -> Int -> CustomMenuLink -> Html Wizard.Msgs.Msg
customMenuItem model index link =
    let
        targetArg =
            if link.newWindow then
                [ target "_blank" ]

            else
                []
    in
    menuLinkSimple model
        (a ([ href link.url, dataCy "menu_custom-link" ] ++ targetArg))
        ("custom-menu-item-" ++ String.fromInt index)
        (fa ("fa-fw " ++ link.icon))
        (gettext link.title model.appState.locale)
        False


menuLinkSimple :
    Model
    -> (List (Html Wizard.Msgs.Msg) -> Html Wizard.Msgs.Msg)
    -> String
    -> Html Wizard.Msgs.Msg
    -> String
    -> Bool
    -> Html Wizard.Msgs.Msg
menuLinkSimple model link itemId itemIcon itemTitle isActive =
    let
        menuItemId =
            "menu_" ++ itemId

        mouseenter =
            onMouseEnter (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.GetElement menuItemId))

        mouseleave =
            onMouseLeave (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.HideElement menuItemId))

        ( submenuStyle, submenuClass ) =
            case ( model.appState.session.sidebarCollapsed, Dict.get menuItemId model.menuModel.submenuPositions ) of
                ( True, Just element ) ->
                    let
                        top =
                            element.element.y - element.viewport.y + (element.element.height / 2)
                    in
                    ( [ style "top" (String.fromFloat top ++ "px") ], "show" )

                _ ->
                    ( [], "" )
    in
    li [ id menuItemId, classList [ ( "active", isActive ) ], mouseenter, mouseleave ]
        [ link
            [ itemIcon
            , span [ class "sidebar-link" ] [ text itemTitle ]
            ]
        , div ([ class "submenu submenu-floating submenu-tooltip", class submenuClass ] ++ submenuStyle)
            [ ul []
                [ li [] [ text itemTitle ]
                ]
            ]
        ]


viewSessionWarning : Model -> Html Wizard.Msgs.Msg
viewSessionWarning model =
    if AppState.sessionExpiresSoon model.appState then
        div [ class "session-warning alert alert-warning" ]
            (viewSessionWarningContent model)

    else
        emptyNode


viewSessionWarningCollapsed : Model -> Html Wizard.Msgs.Msg
viewSessionWarningCollapsed model =
    if AppState.sessionExpiresSoon model.appState then
        let
            itemId =
                "menu_session-warning"

            mouseenter =
                onMouseEnter (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.GetElement itemId))

            mouseleave =
                onMouseLeave (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.HideElement itemId))

            ( submenuStyle, submenuClass ) =
                case Dict.get itemId model.menuModel.submenuPositions of
                    Just element ->
                        let
                            top =
                                element.element.y - element.viewport.y + element.element.height
                        in
                        ( [ style "top" (String.fromFloat top ++ "px") ], "show" )

                    _ ->
                        ( [], "" )

            sessionWarningSubmenu =
                div ([ class "session-warning-submenu", class submenuClass ] ++ submenuStyle)
                    [ div [ class "alert alert-warning" ]
                        (viewSessionWarningContent model)
                    ]
        in
        div
            [ id itemId
            , class "session-warning-collapsed alert alert-warning"
            , mouseenter
            , mouseleave
            ]
            [ faSet "_global.warning" model.appState
            , sessionWarningSubmenu
            ]

    else
        emptyNode


viewSessionWarningContent : Model -> List (Html Wizard.Msgs.Msg)
viewSessionWarningContent model =
    let
        logoutMsg =
            Just (Routing.toUrl model.appState model.appState.route)
                |> Routes.publicLogin
                |> Wizard.Auth.Msgs.LogoutTo
                |> Wizard.Msgs.AuthMsg
    in
    [ text
        (String.format (gettext "Your session expires in %s" model.appState.locale)
            [ AppState.sessionRemainingTime model.appState ]
        )
    , button
        [ onClick logoutMsg
        , class "btn btn-sm btn-warning mt-2"
        ]
        [ text (gettext "Log in again" model.appState.locale) ]
    ]


viewProfileMenu : Model -> Html Wizard.Msgs.Msg
viewProfileMenu model =
    let
        itemId =
            "menu_profile"

        mouseenter =
            onMouseEnter (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.GetElement itemId))

        mouseleave =
            onMouseLeave (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.HideElement itemId))

        ( submenuStyle, submenuClass ) =
            case Dict.get itemId model.menuModel.submenuPositions of
                Just element ->
                    let
                        top =
                            element.element.y - element.viewport.y + element.element.height
                    in
                    ( [ style "top" (String.fromFloat top ++ "px") ], "show" )

                _ ->
                    ( [], "" )

        ( name, role, imageUrl ) =
            case model.appState.config.user of
                Just user ->
                    ( User.fullName user, Role.toReadableString model.appState user.role, User.imageUrl user )

                Nothing ->
                    ( "", "", "" )

        profileInfoInSubmenu =
            if model.appState.session.sidebarCollapsed then
                li [ class "profile-info-submenu" ]
                    [ img [ src imageUrl, class "profile-image" ] []
                    , span [ class "sidebar-link" ]
                        [ span [ class "profile-name" ] [ text name ]
                        , span [ class "profile-role" ] [ text role ]
                        ]
                    ]

            else
                emptyNode

        languageButton =
            if Admin.isEnabled model.appState.config.admin then
                li []
                    [ a
                        [ dataCy "menu_languages"
                        , href "/admin/users/edit/current/language"
                        ]
                        [ faSetFw "menu.language" model.appState
                        , text (gettext "Change language" model.appState.locale)
                        ]
                    ]

            else
                li []
                    [ linkTo model.appState
                        Routes.usersEditLanguageCurrent
                        [ dataCy "menu_languages"
                        ]
                        [ faSetFw "menu.language" model.appState
                        , text (gettext "Change language" model.appState.locale)
                        ]
                    ]
    in
    div [ id itemId, class "profile-info", mouseenter, mouseleave ]
        [ img [ src imageUrl, class "profile-image" ] []
        , span [ class "sidebar-link" ]
            [ span [ class "profile-name" ] [ text name ]
            , span [ class "profile-role" ] [ text role ]
            ]
        , div ([ class "profile-submenu", class submenuClass ] ++ submenuStyle)
            [ ul []
                [ profileInfoInSubmenu
                , li []
                    [ linkTo model.appState
                        Routes.usersEditCurrent
                        [ dataCy "menu_profile" ]
                        [ faSetFw "menu.profile" model.appState
                        , text (gettext "User settings" model.appState.locale)
                        ]
                    ]
                , li []
                    [ linkTo model.appState
                        Routes.commentsIndex
                        [ dataCy "menu_assigned-comments" ]
                        [ faSetFw "menu.assignedComments" model.appState
                        , text (gettext "Assigned comments" model.appState.locale)
                        ]
                    ]
                , languageButton
                , li []
                    [ a
                        [ onClick (Wizard.Msgs.AuthMsg Wizard.Auth.Msgs.Logout)
                        , dataCy "menu_logout"
                        ]
                        [ faSetFw "menu.logout" model.appState
                        , text (gettext "Log out" model.appState.locale)
                        ]
                    ]
                , li [ class "dark dark-border" ]
                    [ a
                        [ onClick (Wizard.Msgs.MenuMsg <| Wizard.Common.Menu.Msgs.SetAboutOpen True)
                        , dataCy "menu_about"
                        ]
                        [ faSetFw "menu.about" model.appState
                        , text (gettext "About" model.appState.locale)
                        ]
                    ]
                , li [ class "dark dark-last" ]
                    [ a
                        [ onClick (Wizard.Msgs.MenuMsg <| Wizard.Common.Menu.Msgs.SetReportIssueOpen True)
                        , dataCy "menu_report-issue"
                        ]
                        [ faSetFw "menu.reportIssue" model.appState
                        , text (gettext "Report issue" model.appState.locale)
                        ]
                    ]
                ]
            ]
        ]


viewCollapseLink : Model -> Html Wizard.Msgs.Msg
viewCollapseLink model =
    if model.appState.session.sidebarCollapsed then
        a [ onLinkClick (Wizard.Msgs.SetSidebarCollapsed False), class "collapse-link" ]
            [ faSet "menu.open" model.appState ]

    else
        a [ onLinkClick (Wizard.Msgs.SetSidebarCollapsed True), class "collapse-link" ]
            [ faSet "menu.collapse" model.appState
            , text (gettext "Collapse sidebar" model.appState.locale)
            ]


viewReportIssueModal : AppState -> Bool -> Html Wizard.Msgs.Msg
viewReportIssueModal appState isOpen =
    let
        supportMailLink =
            a
                [ href <| "mailto:" ++ PrivacyAndSupportConfig.getSupportEmail appState.config.privacyAndSupport
                , dataCy "report-modal_link_support-mail"
                ]
                [ text <| PrivacyAndSupportConfig.getSupportEmail appState.config.privacyAndSupport ]

        modalContent =
            [ p [] [ text (gettext "If you find any problem, you can report an issue here:" appState.locale) ]
            , p []
                [ a
                    [ dataCy "report-modal_link_repository"
                    , class "with-icon"
                    , href <| PrivacyAndSupportConfig.getSupportSiteUrl appState.config.privacyAndSupport
                    , target "_blank"
                    ]
                    [ fa <| PrivacyAndSupportConfig.getSupportSiteIcon appState.config.privacyAndSupport
                    , text <| PrivacyAndSupportConfig.getSupportSiteName appState.config.privacyAndSupport
                    ]
                ]
            , p [] (String.formatHtml (gettext "You can also write us an email to %s." appState.locale) [ supportMailLink ])
            ]

        modalConfig =
            Modal.confirmConfig (gettext "Report Issue" appState.locale)
                |> Modal.confirmConfigContent modalContent
                |> Modal.confirmConfigVisible isOpen
                |> Modal.confirmConfigAction (gettext "OK" appState.locale) (Wizard.Msgs.MenuMsg <| SetReportIssueOpen False)
                |> Modal.confirmConfigCancelShortcutMsg (Wizard.Msgs.MenuMsg <| SetReportIssueOpen False)
                |> Modal.confirmConfigDataCy "report-issue"
    in
    Modal.confirm appState modalConfig


viewAboutModal : AppState -> Bool -> Bool -> ActionResult BuildInfo -> Html Wizard.Msgs.Msg
viewAboutModal appState isOpen recentlyCopied serverBuildInfoActionResult =
    let
        copyButton =
            if ActionResult.isSuccess serverBuildInfoActionResult then
                let
                    copyButtonTooltip =
                        if recentlyCopied then
                            tooltip (gettext "Copied!" appState.locale)

                        else
                            []
                in
                button
                    (class "btn btn-link with-icon"
                        :: onClick (Wizard.Msgs.MenuMsg CopyAbout)
                        :: onMouseLeave (Wizard.Msgs.MenuMsg ClearRecentlyCopied)
                        :: copyButtonTooltip
                    )
                    [ faSet "_global.copy" appState, text (gettext "Copy" appState.locale) ]

            else
                emptyNode

        modalTitle =
            if Admin.isEnabled appState.config.admin then
                String.format (gettext "About %s" appState.locale) [ LookAndFeelConfig.defaultMenuTitle ]

            else
                gettext "About" appState.locale

        modalContent =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text modalTitle ]
                , copyButton
                ]
            , div [ class "modal-body" ]
                [ Page.actionResultView appState (viewAboutModalContent appState) serverBuildInfoActionResult ]
            , div [ class "modal-footer" ]
                [ button
                    [ class "btn btn-primary"
                    , onClick (Wizard.Msgs.MenuMsg (SetAboutOpen False))
                    ]
                    [ text (gettext "OK" appState.locale) ]
                ]
            ]

        modalConfig =
            { modalContent = modalContent
            , visible = isOpen
            , enterMsg = Just (Wizard.Msgs.MenuMsg (SetAboutOpen False))
            , escMsg = Just (Wizard.Msgs.MenuMsg (SetAboutOpen False))
            , dataCy = "about"
            }
    in
    Modal.simple modalConfig


viewAboutModalContent : AppState -> BuildInfo -> Html Wizard.Msgs.Msg
viewAboutModalContent appState serverBuildInfo =
    let
        swaggerUrl =
            appState.apiUrl ++ "/swagger-ui/"

        extraServerInfo =
            [ ( gettext "API URL" appState.locale, a [ href appState.apiUrl, target "_blank" ] [ text appState.apiUrl ] )
            , ( gettext "API Docs" appState.locale, a [ href swaggerUrl, target "_blank" ] [ text swaggerUrl ] )
            ]

        viewComponentVersion component =
            viewBuildInfo appState component.name component []

        componentVersions =
            List.map viewComponentVersion (List.sortBy .name serverBuildInfo.components)
    in
    div []
        ([ viewBuildInfo appState (gettext "Client" appState.locale) BuildInfo.client []
         , viewBuildInfo appState (gettext "Server" appState.locale) serverBuildInfo extraServerInfo
         ]
            ++ componentVersions
        )


viewBuildInfo : AppState -> String -> { a | version : String, builtAt : String } -> List ( String, Html msg ) -> Html msg
viewBuildInfo appState name buildInfo extra =
    let
        viewExtraRow ( title, value ) =
            tr []
                [ td [] [ text title ]
                , td [] [ value ]
                ]

        buildAtValue =
            D.decodeString D.datetime ("\"" ++ buildInfo.builtAt ++ "\"")
                |> Result.map (TimeUtils.toReadableDateTime appState.timeZone)
                |> Result.withDefault buildInfo.builtAt
    in
    table [ class "table table-borderless table-build-info" ]
        [ thead []
            [ tr []
                [ th [ colspan 2 ] [ text name ] ]
            ]
        , tbody []
            ([ tr []
                [ td [] [ text (gettext "Version" appState.locale) ]
                , td [] [ code [] [ text buildInfo.version ] ]
                ]
             , tr []
                [ td [] [ text (gettext "Built at" appState.locale) ]
                , td [] [ em [] [ text buildAtValue ] ]
                ]
             ]
                ++ List.map viewExtraRow extra
            )
        ]
