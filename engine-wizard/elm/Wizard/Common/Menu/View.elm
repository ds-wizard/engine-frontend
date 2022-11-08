module Wizard.Common.Menu.View exposing (view, viewAboutModal, viewLanguagesModal, viewReportIssueModal)

import ActionResult exposing (ActionResult(..))
import Dict
import Gettext exposing (gettext)
import Html exposing (Html, a, button, code, div, em, h5, img, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, colspan, href, id, src, style, target)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import List.Extra as List
import Shared.Auth.Role as Role
import Shared.Components.Badge as Badge
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink exposing (CustomMenuLink)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, fa, faSet, faSetFw)
import String.Format as String
import Wizard.Auth.Msgs
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Feature as Feature
import Wizard.Common.Html exposing (linkTo)
import Wizard.Common.Html.Attribute exposing (dataCy)
import Wizard.Common.Html.Events exposing (onLinkClick)
import Wizard.Common.Menu.Msgs exposing (Msg(..))
import Wizard.Common.View.Modal as Modal
import Wizard.Common.View.Page as Page
import Wizard.Models exposing (Model)
import Wizard.Msgs
import Wizard.Routes as Routes exposing (Route)


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
    }


menuItems : AppState -> List MenuItem
menuItems appState =
    [ MenuItem
        { title = gettext "Apps" appState.locale
        , icon = faSetFw "menu.apps" appState
        , id = "apps"
        , route = Routes.appsIndex
        , isActive = Routes.isAppIndex
        , isVisible = Feature.apps
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
              }
            , { title = gettext "Editors" appState.locale
              , id = "knowledge-models-editors"
              , route = Routes.kmEditorIndex
              , isActive = Routes.isKmEditorIndex
              }
            ]
        }
    , MenuGroup
        { title = gettext "Document Templates" appState.locale
        , icon = faSetFw "menu.templates" appState
        , id = "document-templates"
        , route = Routes.documentTemplatesIndex
        , isActive = Routes.isDocumentTemplatesSubroute
        , isVisible = Feature.templatesView
        , items =
            [ { title = gettext "List" appState.locale
              , id = "documents-list"
              , route = Routes.documentTemplatesIndex
              , isActive = Routes.isDocumentTemplatesIndex
              }
            , { title = gettext "Editors" appState.locale
              , id = "document-editors"
              , route = Routes.documentTemplateEditorsIndex
              , isActive = Routes.isDocumentTemplateEditorsIndex
              }
            ]
        }
    , MenuItem
        { title = gettext "Projects" appState.locale
        , icon = faSetFw "menu.projects" appState
        , id = "projects"
        , route = Routes.projectsIndex appState
        , isActive = Routes.isProjectsIndex
        , isVisible = \a -> Feature.projectsView a && not (Feature.projectImporters a)
        }
    , MenuGroup
        { title = gettext "Projects" appState.locale
        , icon = faSetFw "menu.projects" appState
        , id = "projects"
        , route = Routes.projectsIndex appState
        , isActive = Routes.isProjectSubroute
        , isVisible = \a -> Feature.projectsView a && Feature.projectImporters a
        , items =
            [ { title = gettext "List" appState.locale
              , id = "projects-list"
              , route = Routes.projectsIndex appState
              , isActive = Routes.isProjectsIndex
              }
            , { title = gettext "Importers" appState.locale
              , id = "projects-importers"
              , route = Routes.projectImportersIndex
              , isActive = Routes.isProjectImportersIndex
              }
            ]
        }
    , MenuItem
        { title = gettext "Documents" appState.locale
        , icon = faSetFw "menu.documents" appState
        , id = "documents"
        , route = Routes.documentsIndex
        , isActive = Routes.isDocumentsIndex
        , isVisible = Feature.documentsView
        }
    , MenuGroup
        { title = gettext "Dev" appState.locale
        , icon = faSetFw "menu.dev" appState
        , id = "dev"
        , route = Routes.devOperations
        , isActive = Routes.isDevSubroute
        , isVisible = Feature.dev
        , items =
            [ { title = gettext "Operations" appState.locale
              , id = "dev-operations"
              , route = Routes.devOperations
              , isActive = Routes.isDevOperations
              }
            , { title = gettext "Persistent Commands" appState.locale
              , id = "dev-persistent-commands"
              , route = Routes.persistentCommandsIndex
              , isActive = Routes.isPersistentCommandsIndex
              }
            ]
        }
    , MenuGroup
        { title = gettext "Administration" appState.locale
        , icon = faSetFw "menu.administration" appState
        , id = "administration"
        , route = Routes.settingsDefault
        , isActive = Routes.isSettingsSubroute
        , isVisible = Feature.settings
        , items =
            [ { title = gettext "Settings" appState.locale
              , id = "system-settings"
              , route = Routes.settingsDefault
              , isActive = Routes.isSettingsRoute
              }
            , { title = gettext "Users" appState.locale
              , id = "users"
              , route = Routes.usersIndex
              , isActive = Routes.isUsersIndex
              }
            , { title = gettext "Locales" appState.locale
              , id = "system-locales"
              , route = Routes.localesIndex
              , isActive = Routes.isLocalesRoute
              }
            ]
        }
    ]


view : Model -> Html Wizard.Msgs.Msg
view model =
    div [ class "side-navigation", classList [ ( "side-navigation-collapsed", model.appState.session.sidebarCollapsed ) ] ]
        [ viewLogo model
        , viewMenu model
        , viewProfileMenu model
        , viewCollapseLink model
        ]


viewLogo : Model -> Html Wizard.Msgs.Msg
viewLogo model =
    let
        logoImg =
            span [ class "logo-full", dataCy "nav_app-title-short" ]
                [ span [] [ text <| LookAndFeelConfig.getAppTitleShort model.appState.config.lookAndFeel ] ]
    in
    linkTo model.appState Routes.appHome [ class "logo" ] [ logoImg ]


viewMenu : Model -> Html Wizard.Msgs.Msg
viewMenu model =
    let
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
        (defaultMenuItems ++ [ space ] ++ customMenuItems)


defaultMenuItem : Model -> MenuItem -> Html Wizard.Msgs.Msg
defaultMenuItem model item =
    case item of
        MenuGroup menuGroup ->
            let
                viewGroupItem groupItem =
                    li [ classList [ ( "active", groupItem.isActive model.appState.route ) ] ]
                        [ linkTo model.appState
                            groupItem.route
                            []
                            [ text groupItem.title ]
                        ]

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
                            ( [ style "top" (String.fromFloat element.element.y ++ "px") ], "show" )

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
                    [ ul [] (submenuHeading :: List.map viewGroupItem menuGroup.items) ]
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
        link.title
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
                            element.element.y + (element.element.height / 2)
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
                            element.element.y + element.element.height
                    in
                    ( [ style "top" (String.fromFloat top ++ "px") ], "show" )

                _ ->
                    ( [], "" )

        ( name, role, imageUrl ) =
            case model.appState.session.user of
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

        langaugeButton =
            if List.length model.appState.config.locales < 2 then
                emptyNode

            else
                li []
                    [ a
                        [ onClick (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.SetLanguagesOpen True))
                        , dataCy "menu_languages"
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
                        , text (gettext "Edit profile" model.appState.locale)
                        ]
                    ]
                , langaugeButton
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
            [ p [] [ text (gettext "If you find any problem, the best way to report it is to open an issue in our GitHub repository." appState.locale) ]
            , p []
                [ a
                    [ dataCy "report-modal_link_repository"
                    , class "with-icon"
                    , href <| PrivacyAndSupportConfig.getSupportRepositoryUrl appState.config.privacyAndSupport
                    , target "_blank"
                    ]
                    [ faSet "report.repository" appState
                    , text <| PrivacyAndSupportConfig.getSupportRepositoryName appState.config.privacyAndSupport
                    ]
                ]
            , p [] (String.formatHtml (gettext "You can also write us an email to %s." appState.locale) [ supportMailLink ])
            ]

        modalConfig =
            { modalTitle = gettext "Report issue" appState.locale
            , modalContent = modalContent
            , visible = isOpen
            , actionResult = Unset
            , actionName = gettext "OK" appState.locale
            , actionMsg = Wizard.Msgs.MenuMsg <| SetReportIssueOpen False
            , cancelMsg = Nothing
            , dangerous = False
            , dataCy = "report-issue"
            }
    in
    Modal.confirm appState modalConfig


viewAboutModal : AppState -> Bool -> ActionResult BuildInfo -> Html Wizard.Msgs.Msg
viewAboutModal appState isOpen serverBuildInfoActionResult =
    let
        modalContent =
            Page.actionResultView appState (viewAboutModalContent appState) serverBuildInfoActionResult

        modalConfig =
            { modalTitle = gettext "About" appState.locale
            , modalContent = [ modalContent ]
            , visible = isOpen
            , actionResult = Unset
            , actionName = gettext "OK" appState.locale
            , actionMsg = Wizard.Msgs.MenuMsg <| SetAboutOpen False
            , cancelMsg = Nothing
            , dangerous = False
            , dataCy = "about"
            }
    in
    Modal.confirm appState modalConfig


viewAboutModalContent : AppState -> BuildInfo -> Html Wizard.Msgs.Msg
viewAboutModalContent appState serverBuildInfo =
    let
        swaggerUrl =
            appState.apiUrl ++ "/swagger-ui/"

        extraClientInfo =
            [ ( gettext "Style Version" appState.locale, code [ id "client-style-version" ] [] )
            ]

        extraServerInfo =
            [ ( gettext "API URL" appState.locale, a [ href appState.apiUrl, target "_blank" ] [ text appState.apiUrl ] )
            , ( gettext "API Docs" appState.locale, a [ href swaggerUrl, target "_blank" ] [ text swaggerUrl ] )
            ]
    in
    div []
        [ viewBuildInfo appState (gettext "Client" appState.locale) BuildInfo.client extraClientInfo
        , viewBuildInfo appState (gettext "Server" appState.locale) serverBuildInfo extraServerInfo
        ]


viewBuildInfo : AppState -> String -> BuildInfo -> List ( String, Html msg ) -> Html msg
viewBuildInfo appState name buildInfo extra =
    let
        viewExtraRow ( title, value ) =
            tr []
                [ td [] [ text title ]
                , td [] [ value ]
                ]
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
                , td [] [ em [] [ text buildInfo.builtAt ] ]
                ]
             ]
                ++ List.map viewExtraRow extra
            )
        ]


viewLanguagesModal : AppState -> Bool -> Html Wizard.Msgs.Msg
viewLanguagesModal appState visible =
    let
        selectedLocale =
            case appState.selectedLocale of
                Just selected ->
                    List.find (\locale -> locale.code == selected) appState.config.locales
                        |> Maybe.map .code

                Nothing ->
                    Nothing

        viewLocale locale =
            let
                defaultBadge =
                    if locale.defaultLocale then
                        Badge.info [ class "ms-2" ] [ text (gettext "default" appState.locale) ]

                    else
                        emptyNode

                selected =
                    if Just locale.code == selectedLocale then
                        faSet "locale.selected" appState

                    else
                        emptyNode
            in
            div [ class "nav-link cursor-pointer", onClick (Wizard.Msgs.SetLocale locale.code) ]
                [ selected
                , text locale.name
                , defaultBadge
                ]

        content =
            [ div [ class "modal-header" ]
                [ h5 [ class "modal-title" ] [ text (gettext "Change language" appState.locale) ]
                , button [ class "btn-close", onClick (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.SetLanguagesOpen False)) ] []
                ]
            , div [ class "modal-body" ]
                [ div [ class "nav flex-column nav-pills nav-languages" ]
                    (List.map viewLocale (List.sortBy .name appState.config.locales))
                ]
            ]
    in
    Modal.simple
        { modalContent = content
        , visible = visible
        , dataCy = "languages"
        }
