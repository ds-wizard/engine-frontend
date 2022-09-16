module Wizard.Common.Menu.View exposing (view, viewAboutModal, viewReportIssueModal)

import ActionResult exposing (ActionResult(..))
import Dict
import Html exposing (Html, a, code, div, em, img, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (class, classList, colspan, href, id, src, style, target)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Shared.Auth.Role as Role
import Shared.Data.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Shared.Data.BootstrapConfig.LookAndFeelConfig.CustomMenuLink exposing (CustomMenuLink)
import Shared.Data.BootstrapConfig.PrivacyAndSupportConfig as PrivacyAndSupportConfig
import Shared.Data.BuildInfo as BuildInfo exposing (BuildInfo)
import Shared.Data.User as User
import Shared.Html exposing (emptyNode, fa, faSet, faSetFw)
import Shared.Locale exposing (l, lh, lx)
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


l_ : String -> AppState -> String
l_ =
    l "Wizard.Common.Menu.View"


lh_ : String -> List (Html msg) -> AppState -> List (Html msg)
lh_ =
    lh "Wizard.Common.Menu.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Common.Menu.View"


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
        { title = l_ "menu.apps" appState
        , icon = faSetFw "menu.apps" appState
        , id = "apps"
        , route = Routes.appsIndex
        , isActive = Routes.isAppIndex
        , isVisible = Feature.apps
        }
    , MenuItem
        { title = l_ "menu.users" appState
        , icon = faSetFw "menu.users" appState
        , id = "users"
        , route = Routes.usersIndex
        , isActive = Routes.isUsersIndex
        , isVisible = Feature.usersView
        }
    , MenuGroup
        { title = l_ "menu.knowledgeModels" appState
        , icon = faSetFw "menu.knowledgeModels" appState
        , id = "knowledge-models"
        , route = Routes.knowledgeModelsIndex
        , isActive = Routes.isKnowledgeModelsSubroute
        , isVisible = Feature.knowledgeModelsImport
        , items =
            [ { title = l_ "menu.knowledgeModels.list" appState
              , id = "knowledge-models-list"
              , route = Routes.knowledgeModelsIndex
              , isActive = Routes.isKnowledgeModelsIndex
              }
            , { title = l_ "menu.knowledgeModels.editors" appState
              , id = "knowledge-models-editors"
              , route = Routes.kmEditorIndex
              , isActive = Routes.isKmEditorIndex
              }
            ]
        }
    , MenuItem
        { title = l_ "menu.projects" appState
        , icon = faSetFw "menu.projects" appState
        , id = "projects"
        , route = Routes.projectsIndex appState
        , isActive = Routes.isProjectsIndex
        , isVisible = \a -> Feature.projectsView a && not (Feature.projectImporters a)
        }
    , MenuGroup
        { title = l_ "menu.projects" appState
        , icon = faSetFw "menu.projects" appState
        , id = "projects"
        , route = Routes.projectsIndex appState
        , isActive = Routes.isProjectSubroute
        , isVisible = \a -> Feature.projectsView a && Feature.projectImporters a
        , items =
            [ { title = l_ "menu.projects.list" appState
              , id = "projects-list"
              , route = Routes.projectsIndex appState
              , isActive = Routes.isProjectsIndex
              }
            , { title = l_ "menu.projects.importers" appState
              , id = "projects-importers"
              , route = Routes.projectImportersIndex
              , isActive = Routes.isProjectImportersIndex
              }
            ]
        }
    , MenuGroup
        { title = l_ "menu.documents" appState
        , icon = faSetFw "menu.documents" appState
        , id = "documents"
        , route = Routes.documentsIndex
        , isActive = Routes.isDocumentsSubroute
        , isVisible = Feature.documentsView
        , items =
            [ { title = l_ "menu.documents.list" appState
              , id = "documents-list"
              , route = Routes.documentsIndex
              , isActive = Routes.isDocumentsIndex
              }
            , { title = l_ "menu.documents.templates" appState
              , id = "document-templates"
              , route = Routes.templatesIndex
              , isActive = Routes.isTemplateIndex
              }
            ]
        }
    , MenuItem
        { title = l_ "menu.documentTemplates" appState
        , icon = faSetFw "menu.templates" appState
        , id = "document-templates"
        , route = Routes.templatesIndex
        , isActive = Routes.isTemplateIndex
        , isVisible = \a -> Feature.templatesView a && not (Feature.documentsView a)
        }
    , MenuGroup
        { title = l_ "menu.dev" appState
        , icon = faSetFw "menu.dev" appState
        , id = "dev"
        , route = Routes.devOperations
        , isActive = Routes.isDevSubroute
        , isVisible = Feature.dev
        , items =
            [ { title = l_ "menu.dev.operations" appState
              , id = "dev-operations"
              , route = Routes.devOperations
              , isActive = Routes.isDevOperations
              }
            , { title = l_ "menu.dev.persistentCommands" appState
              , id = "dev-persistent-commands"
              , route = Routes.persistentCommandsIndex
              , isActive = Routes.isPersistentCommandsIndex
              }
            ]
        }
    , MenuItem
        { title = l_ "menu.settings" appState
        , icon = faSetFw "menu.settings" appState
        , id = "settings"
        , route = Routes.settingsDefault
        , isActive = Routes.isSettingsRoute
        , isVisible = Feature.settings
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

                mouseenter =
                    onMouseEnter (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.GetElement menuGroup.id))

                mouseleave =
                    onMouseLeave (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.HideElement menuGroup.id))

                ( submenuStyle, submenuExtraClass ) =
                    case Dict.get menuGroup.id model.menuModel.submenuPositions of
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
            li [ id menuGroup.id, classList [ ( "active", menuGroup.isActive model.appState.route ) ], mouseenter, mouseleave ]
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
        mouseenter =
            onMouseEnter (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.GetElement itemId))

        mouseleave =
            onMouseLeave (Wizard.Msgs.MenuMsg (Wizard.Common.Menu.Msgs.HideElement itemId))

        ( submenuStyle, submenuClass ) =
            case ( model.appState.session.sidebarCollapsed, Dict.get itemId model.menuModel.submenuPositions ) of
                ( True, Just element ) ->
                    let
                        top =
                            element.element.y + (element.element.height / 2)
                    in
                    ( [ style "top" (String.fromFloat top ++ "px") ], "show" )

                _ ->
                    ( [], "" )
    in
    li [ id itemId, classList [ ( "active", isActive ) ], mouseenter, mouseleave ]
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
            "profile"

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
                        []
                        [ faSetFw "menu.profile" model.appState
                        , lx_ "profileMenu.edit" model.appState
                        ]
                    ]
                , li []
                    [ a [ onClick (Wizard.Msgs.AuthMsg Wizard.Auth.Msgs.Logout) ]
                        [ faSetFw "menu.logout" model.appState
                        , lx_ "profileMenu.logout" model.appState
                        ]
                    ]
                , li [ class "dark dark-border" ]
                    [ a [ onClick (Wizard.Msgs.MenuMsg <| Wizard.Common.Menu.Msgs.SetAboutOpen True) ]
                        [ faSetFw "menu.about" model.appState
                        , lx_ "profileMenu.about" model.appState
                        ]
                    ]
                , li [ class "dark dark-last" ]
                    [ a [ onClick (Wizard.Msgs.MenuMsg <| Wizard.Common.Menu.Msgs.SetReportIssueOpen True) ]
                        [ faSetFw "menu.reportIssue" model.appState
                        , lx_ "profileMenu.reportIssue" model.appState
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
            , lx_ "sidebar.collapse" model.appState
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
            [ p [] [ lx_ "reportModal.info" appState ]
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
            , p [] (lh_ "reportModal.writeUs" [ supportMailLink ] appState)
            ]

        modalConfig =
            { modalTitle = l_ "reportModal.title" appState
            , modalContent = modalContent
            , visible = isOpen
            , actionResult = Unset
            , actionName = l_ "reportModal.action" appState
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
            { modalTitle = l_ "about.title" appState
            , modalContent = [ modalContent ]
            , visible = isOpen
            , actionResult = Unset
            , actionName = l_ "about.action" appState
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
            [ ( l_ "about.styleVersion" appState, code [ id "client-style-version" ] [] )
            ]

        extraServerInfo =
            [ ( l_ "about.apiUrl" appState, a [ href appState.apiUrl, target "_blank" ] [ text appState.apiUrl ] )
            , ( l_ "about.apiDocs" appState, a [ href swaggerUrl, target "_blank" ] [ text swaggerUrl ] )
            ]
    in
    div []
        [ viewBuildInfo appState (l_ "about.client" appState) BuildInfo.client extraClientInfo
        , viewBuildInfo appState (l_ "about.server" appState) serverBuildInfo extraServerInfo
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
                [ td [] [ lx_ "about.version" appState ]
                , td [] [ code [] [ text buildInfo.version ] ]
                ]
             , tr []
                [ td [] [ lx_ "about.builtAt" appState ]
                , td [] [ em [] [ text buildInfo.builtAt ] ]
                ]
             ]
                ++ List.map viewExtraRow extra
            )
        ]
