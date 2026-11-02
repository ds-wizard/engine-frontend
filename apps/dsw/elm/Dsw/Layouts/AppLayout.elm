module Dsw.Layouts.AppLayout exposing
    ( AppConfig
    , app
    , destinations
    , misconfigured
    , mixedApp
    , public
    )

{-| The client's frame: `Common.Pages.Layout` with this client's
navigation and its pages' names.
-}

import Browser exposing (Document)
import Common.Components.AppFrame as AppFrame exposing (MenuEntry)
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Pages.Layout as Layout
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import Ui.Theme as Theme


type alias AppConfig msg =
    { frame : AppFrame.Model
    , frameMsg : AppFrame.Msg -> msg
    , setMode : Theme.Mode -> msg
    , logoutMsg : msg
    , openAboutMsg : msg
    , openCommandPaletteMsg : msg
    , openNewsMsg : msg
    , openReportIssueMsg : msg
    , footerNotice : Maybe (Html msg)
    , overlays : List (Html msg)
    , route : Routes.Route
    }


layoutConfig : AppState -> AppConfig msg -> Layout.AppConfig msg
layoutConfig appState config =
    { frame = config.frame
    , frameMsg = config.frameMsg
    , setMode = config.setMode
    , logoutMsg = config.logoutMsg
    , openAboutMsg = config.openAboutMsg
    , openCommandPaletteMsg = config.openCommandPaletteMsg
    , openNewsMsg = config.openNewsMsg
    , openReportIssueMsg = config.openReportIssueMsg
    , footerNotice = config.footerNotice
    , footerActions = []
    , sidePanel = Nothing
    , overlays = config.overlays
    , title = Layout.pageName appState config.route
    , currentUrl = Routes.toUrl appState config.route
    , context = Nothing
    , menu = menuEntries appState config.route
    , assignedComments = True
    , customMenuLinks = True
    }


misconfigured : AppState -> Document msg
misconfigured =
    Layout.misconfigured


app : AppState -> AppConfig msg -> Html msg -> Document msg
app appState config =
    Layout.app appState (layoutConfig appState config)


mixedApp : AppState -> AppConfig msg -> Html msg -> Document msg
mixedApp appState config =
    Layout.mixedApp appState (layoutConfig appState config)


public : AppState -> { route : Routes.Route, overlays : List (Html msg) } -> Html msg -> Document msg
public appState config =
    Layout.public appState { title = Layout.pageName appState config.route, overlays = config.overlays }



-- MENU


{-| Where the palette can take you: the navigation's own entries, so a page
that is in one is in the other, with the permissions already applied.
-}
destinations : AppState -> List { id : String, label : String, section : String, url : String, leading : Maybe (Html Never) }
destinations appState =
    -- Nothing is the current page in a list of places to go.
    AppFrame.destinations (menuEntries appState Routes.NotFoundRoute)


{-| The navigation, as the wizard has it: the same entries, icons and
permissions. A group with no visible children is left out, and a group with one
is shown as that one link under the group's icon, named by its `soloTitle` —
"Knowledge Models" rather than "List", which means nothing on its own.
-}
menuEntries : AppState -> Routes.Route -> List (MenuEntry msg)
menuEntries appState route =
    let
        simpleItem entryId icon title targetRoute isActive isVisible =
            Layout.menuItem
                { id = entryId
                , icon = icon
                , label = title
                , url = Routes.toUrl appState targetRoute
                , active = isActive route
                , visible = isVisible
                }

        groupEntry entryId icon title isActive items =
            Layout.menuGroup
                { id = entryId, icon = icon, label = title, active = isActive route }
                (List.map
                    (\groupItem ->
                        { id = groupItem.id
                        , title = groupItem.title
                        , soloTitle = groupItem.soloTitle
                        , url = Routes.toUrl appState groupItem.route
                        , active = groupItem.isActive route
                        , visible = groupItem.isVisible appState
                        }
                    )
                    items
                )
    in
    List.concat
        [ simpleItem "dashboard" "fa-house" (gettext "Dashboard" appState.locale) Routes.dashboard Routes.isDashboard True
        , simpleItem "tenants" "fa-server" "Tenants" Routes.tenantsIndex Routes.isTenantIndex (Feature.tenants appState)
        , groupEntry "knowledge-models"
            "fa-sitemap"
            (gettext "Knowledge Models" appState.locale)
            Routes.isKnowledgeModelsSubroute
            [ { title = gettext "List" appState.locale
              , soloTitle = Just (gettext "Knowledge Models" appState.locale)
              , route = Routes.knowledgeModelsIndex
              , id = "knowledge-models-list"
              , isActive = Routes.isKnowledgeModelsIndex
              , isVisible = Feature.knowledgeModelsManage
              }
            , { title = gettext "Editors" appState.locale
              , soloTitle = Just (gettext "Knowledge Model Editors" appState.locale)
              , route = Routes.kmEditorIndex
              , id = "knowledge-models-editors"
              , isActive = Routes.isKmEditorIndex
              , isVisible = Feature.knowledgeModelEditorsView
              }
            , { title = gettext "Secrets" appState.locale
              , soloTitle = Just (gettext "Knowledge Model Secrets" appState.locale)
              , route = Routes.knowledgeModelSecrets
              , id = "knowledge-model-secrets"
              , isActive = Routes.isKnowledgeModelSecrets
              , isVisible = Feature.knowledgeModelSecrets
              }
            ]
        , groupEntry "document-templates"
            "fa-file-code"
            (gettext "Document Templates" appState.locale)
            Routes.isDocumentTemplatesSubroute
            [ { title = gettext "List" appState.locale
              , soloTitle = Just (gettext "Document Templates" appState.locale)
              , route = Routes.documentTemplatesIndex
              , id = "documents-list"
              , isActive = Routes.isDocumentTemplatesIndex
              , isVisible = Feature.documentTemplatesManage
              }
            , { title = gettext "Editors" appState.locale
              , soloTitle = Just (gettext "Document Template Editors" appState.locale)
              , route = Routes.documentTemplateEditorsIndex
              , id = "document-editors"
              , isActive = Routes.isDocumentTemplateEditorsIndex
              , isVisible = Feature.documentTemplateEditorsView
              }
            ]
        , groupEntry "projects"
            "fa-folder"
            (gettext "Projects" appState.locale)
            Routes.isProjectSubroute
            [ { title = gettext "List" appState.locale
              , soloTitle = Just (gettext "Projects" appState.locale)
              , route = Routes.projectsIndex appState
              , id = "projects-list"
              , isActive = Routes.isProjectsIndex
              , isVisible = Feature.projectsView
              }
            , { title = gettext "Files" appState.locale
              , soloTitle = Just (gettext "Project Files" appState.locale)
              , route = Routes.projectFilesIndex
              , id = "projects-files"
              , isActive = Routes.isProjectFilesIndex
              , isVisible = Feature.projectFiles
              }
            , { title = gettext "Documents" appState.locale
              , soloTitle = Nothing
              , route = Routes.documentsIndex
              , id = "documents"
              , isActive = Routes.isDocumentsIndex
              , isVisible = Feature.documentsView
              }
            ]

        -- Everybody may see the groups and create them, so they are not
        -- administration.
        , simpleItem "user-groups" "fa-user-group" (gettext "User Groups" appState.locale) Routes.userGroupsIndex Routes.isUserGroupsSubroute (Feature.userGroupsView appState)
        , groupEntry "dev"
            "fa-laptop-code"
            "Dev"
            Routes.isDevSubroute
            [ { title = "Operations"
              , soloTitle = Just "Dev Operations"
              , route = Routes.devOperations
              , id = "dev-operations"
              , isActive = Routes.isDevOperations
              , isVisible = Feature.dev
              }
            , { title = "Persistent Commands"
              , soloTitle = Nothing
              , route = Routes.persistentCommandsIndex
              , id = "dev-persistent-commands"
              , isActive = Routes.isPersistentCommandsIndex
              , isVisible = Feature.dev
              }
            ]
        , groupEntry "administration"
            "fa-gear"
            (gettext "Administration" appState.locale)
            Routes.isSettingsSubroute
            [ { title = gettext "Settings" appState.locale
              , soloTitle = Nothing
              , route = Routes.settingsDefault appState
              , id = "system-settings"
              , isActive = Routes.isSettingsRoute
              , isVisible = Feature.settingsAny
              }
            , { title = gettext "Users" appState.locale
              , soloTitle = Nothing
              , route = Routes.usersIndex
              , id = "users"
              , isActive = Routes.isUsersIndex
              , isVisible = Feature.usersManage
              }
            ]
        ]
