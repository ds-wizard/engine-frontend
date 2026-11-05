module Common.Pages.Layout exposing
    ( AppConfig
    , MenuGroupItem
    , app
    , documentTitle
    , menuGroup
    , menuItem
    , misconfigured
    , mixedApp
    , pageName
    , public
    )

{-| The documents the application renders: the frame around a signed-in page,
the public frame, and the page shown when the application cannot start.

Shared by the clients: each builds its own navigation (with `menuItem` and
`menuGroup`) and names its own pages, and hands both in `AppConfig` with
everything else the frame sends — its own state, the appearance, the account
menu's actions. `pageName` names the pages they share.

**Public pages draw their own frame.** `Ui.Page.Public` owns the page's `main`
and puts the page's title inside its panel, and only the page knows its title,
so a public page renders its own frame — `publicApp` for a page with a template
of its own, `publicPanel` for a form or a message in a panel — and `public`
only adds the document title and the banners. The authenticated frame is the
other way round, because `Ui.AppShell` gives the page a region and the page
fills it.

**The document title names the page.** "Create Project | Wizard": the
page first, so a window list or a tab too narrow for both still says which page
it is, then the instance's application title (2.4.2). The page's name comes
from the route, not from the page, so it is there while the page is still
loading; it is the name the page gives its own `h1` wherever that does not
depend on the data (a knowledge model's page is "Knowledge Model", not its
name), and a page within a section is "Section: Page", as the settings menu
says it on a narrow screen.

-}

import Browser exposing (Document)
import Common.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig
import Common.Api.Models.Role as Role
import Common.Api.Models.User as User
import Common.Components.AppFrame as AppFrame exposing (MenuEntry)
import Common.Components.Page as Page
import Common.Components.PublicFrame as PublicFrame
import Common.Components.UserAvatar as UserAvatar
import Common.Data.AppState exposing (AppState)
import Common.Data.Feature as Feature
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Common.Pages.Dev.Routes
import Common.Pages.DocumentTemplateEditors.Routes as DocumentTemplateEditorsRoutes
import Common.Pages.DocumentTemplates.Detail.Routes as DocumentTemplateDetailRoutes
import Common.Pages.DocumentTemplates.Routes as DocumentTemplatesRoutes
import Common.Pages.KMEditor.Routes as KMEditorRoutes
import Common.Pages.KnowledgeModels.Detail.Routes as KnowledgeModelDetailRoutes
import Common.Pages.KnowledgeModels.Routes as KnowledgeModelsRoutes
import Common.Pages.Projects.Routes as ProjectsRoutes
import Common.Pages.Public.Layout as PublicLayout
import Common.Pages.Public.Routes as PublicRoutes
import Common.Pages.Settings.Routes as SettingsRoutes
import Common.Pages.Tenants.Routes as TenantsRoutes
import Common.Pages.UserGroups.Routes as UserGroupsRoutes
import Common.Pages.Users.Edit.Routes as UserEditRoutes
import Common.Pages.Users.Routes as UsersRoutes
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import Ui.Icon as Icon
import Ui.Menu as Menu
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

    -- Controls at the foot of the navigation that open a panel beside the
    -- page, and that panel while it is open (`AppFrame.ViewConfig`), where a
    -- client has any.
    , footerActions : List (Html msg)
    , sidePanel : Maybe (Html msg)
    , overlays : List (Html msg)

    -- What the page is called (`pageName` for a shared page), for the document
    -- title.
    , title : String

    -- The address of the page, so that "Log In" on a page anyone may see comes
    -- back to it.
    , currentUrl : String

    -- Where the navigation belongs, and the way to the other such places
    -- (`AppFrame.Context`), for an application that has several.
    , context : Maybe (AppFrame.Context msg)
    , menu : List (MenuEntry msg)

    -- Whether the account menu links to the comments assigned to the user.
    , assignedComments : Bool

    -- Whether the navigation ends with the links configured in Dashboard &
    -- Menu. Off only where the frame is to have no links at all.
    , customMenuLinks : Bool
    }


documentTitle : AppState -> String -> String
documentTitle appState title =
    title ++ " | " ++ LookAndFeelConfig.getAppTitle appState.config.lookAndFeel


{-| What the page at a route is called, in the page's own words.
-}
pageName : AppState -> Routes.Route -> String
pageName appState route =
    let
        l =
            appState.locale

        within section page =
            section ++ ": " ++ page
    in
    case route of
        Routes.DashboardRoute ->
            gettext "Dashboard" l

        Routes.CommentsRoute _ _ ->
            gettext "Assigned Comments" l

        Routes.DevRoute devRoute ->
            case devRoute of
                Common.Pages.Dev.Routes.OperationsRoute _ ->
                    "Dev Operations"

                Common.Pages.Dev.Routes.PersistentCommandsDetail _ ->
                    "Persistent Command"

                Common.Pages.Dev.Routes.PersistentCommandsIndex _ _ ->
                    "Persistent Commands"

        Routes.DocumentsRoute _ ->
            gettext "Project Documents" l

        Routes.DocumentTemplateEditorsRoute editorsRoute ->
            case editorsRoute of
                DocumentTemplateEditorsRoutes.CreateRoute _ _ ->
                    gettext "Create Document Template" l

                DocumentTemplateEditorsRoutes.IndexRoute _ ->
                    gettext "Document Template Editors" l

                DocumentTemplateEditorsRoutes.EditorRoute _ _ ->
                    gettext "Document Template Editor" l

        Routes.DocumentTemplatesRoute templatesRoute ->
            case templatesRoute of
                DocumentTemplatesRoutes.DetailRoute _ DocumentTemplateDetailRoutes.Readme ->
                    gettext "Document Template" l

                DocumentTemplatesRoutes.DetailRoute _ DocumentTemplateDetailRoutes.Locales ->
                    within (gettext "Document Template" l) (gettext "Locales" l)

                DocumentTemplatesRoutes.ImportRoute _ ->
                    gettext "Import Document Template" l

                DocumentTemplatesRoutes.IndexRoute _ ->
                    gettext "Document Templates" l

        Routes.KMEditorRoute kmEditorRoute ->
            case kmEditorRoute of
                KMEditorRoutes.CreateRoute _ _ ->
                    gettext "Create knowledge model" l

                KMEditorRoutes.EditorRoute _ _ ->
                    gettext "Knowledge Model Editor" l

                KMEditorRoutes.IndexRoute _ ->
                    gettext "Knowledge Model Editors" l

                KMEditorRoutes.MigrationRoute _ ->
                    gettext "Migration" l

                KMEditorRoutes.PublishRoute _ ->
                    gettext "Publish new version" l

        Routes.KnowledgeModelSecretsRoute ->
            gettext "Knowledge Model Secrets" l

        Routes.KnowledgeModelsRoute knowledgeModelsRoute ->
            case knowledgeModelsRoute of
                KnowledgeModelsRoutes.DetailRoute _ KnowledgeModelDetailRoutes.Readme ->
                    gettext "Knowledge Model" l

                KnowledgeModelsRoutes.DetailRoute _ KnowledgeModelDetailRoutes.Locales ->
                    within (gettext "Knowledge Model" l) (gettext "Locales" l)

                KnowledgeModelsRoutes.ImportRoute _ ->
                    gettext "Import Knowledge Model" l

                KnowledgeModelsRoutes.IndexRoute _ ->
                    gettext "Knowledge Models" l

                KnowledgeModelsRoutes.PreviewRoute _ _ ->
                    gettext "Preview" l

                KnowledgeModelsRoutes.ResourcePageRoute _ _ ->
                    gettext "Resource Page" l

                KnowledgeModelsRoutes.CompareRoute _ ->
                    gettext "Compare Knowledge Models" l

        Routes.UserGroupsRoute userGroupsRoute ->
            case userGroupsRoute of
                UserGroupsRoutes.Index _ _ ->
                    gettext "User Groups" l

                UserGroupsRoutes.Create ->
                    gettext "Create User Group" l

                UserGroupsRoutes.Detail _ _ ->
                    gettext "User Group" l

        Routes.ProjectsRoute projectsRoute ->
            case projectsRoute of
                ProjectsRoutes.CreateRoute _ _ ->
                    gettext "Create Project" l

                ProjectsRoutes.CreateMigrationRoute _ ->
                    gettext "Migrate Project" l

                ProjectsRoutes.DetailRoute _ _ ->
                    gettext "Project" l

                ProjectsRoutes.IndexRoute _ _ _ _ _ _ _ _ _ _ ->
                    gettext "Projects" l

                ProjectsRoutes.ImportRoute _ _ ->
                    gettext "Import" l

                ProjectsRoutes.DocumentDownloadRoute _ _ ->
                    gettext "Downloading" l

                ProjectsRoutes.FileDownloadRoute _ _ ->
                    gettext "Downloading" l

        Routes.ProjectFilesRoute _ ->
            gettext "Project Files" l

        Routes.PublicRoute publicRoute ->
            case publicRoute of
                PublicRoutes.ForgottenPasswordRoute ->
                    gettext "Forgotten Password" l

                PublicRoutes.ForgottenPasswordConfirmationRoute _ _ ->
                    gettext "Password Recovery" l

                PublicRoutes.LoginRoute _ ->
                    gettext "Log In" l

                PublicRoutes.LogoutSuccessful ->
                    gettext "Logged out" l

                PublicRoutes.OpenIdCallback _ _ _ _ _ ->
                    gettext "Log In" l

                PublicRoutes.SignupRoute ->
                    gettext "Sign Up" l

                PublicRoutes.SignupConfirmationRoute _ _ ->
                    gettext "Sign Up" l

        Routes.SettingsRoute settingsRoute ->
            within (gettext "Settings" l) <|
                case settingsRoute of
                    SettingsRoutes.AuthenticationRoute ->
                        gettext "Authentication" l

                    SettingsRoutes.UsersRoute ->
                        gettext "Users" l

                    SettingsRoutes.OpenIdRoute ->
                        gettext "OpenID" l

                    SettingsRoutes.OpenIdCreateRoute ->
                        gettext "Create OpenID Config" l

                    SettingsRoutes.OpenIdDetailRoute _ ->
                        gettext "Edit OpenID Config" l

                    SettingsRoutes.RolesRoute ->
                        gettext "Roles" l

                    SettingsRoutes.RoleCreateRoute ->
                        gettext "Create Role" l

                    SettingsRoutes.RoleDetailRoute _ ->
                        gettext "Edit Role" l

                    SettingsRoutes.SupportRoute ->
                        gettext "Support" l

                    SettingsRoutes.FeaturesRoute ->
                        gettext "Features" l

                    SettingsRoutes.PluginsRoute ->
                        gettext "Plugins" l

                    SettingsRoutes.PluginSettingsRoute _ ->
                        gettext "Plugin settings" l

                    SettingsRoutes.LoginScreenRoute ->
                        gettext "Login Screen" l

                    SettingsRoutes.DashboardAndMenuRoute ->
                        gettext "Dashboard & Menu" l

                    SettingsRoutes.LookAndFeelRoute ->
                        gettext "Look & Feel" l

                    SettingsRoutes.RegistryRoute ->
                        gettext "Registry" l

                    SettingsRoutes.ProjectsRoute ->
                        gettext "Projects" l

                    SettingsRoutes.SubmissionRoute ->
                        gettext "Document Submission" l

                    SettingsRoutes.UsageRoute ->
                        gettext "Usage" l

                    SettingsRoutes.LocalesRoute kind page ->
                        case ( kind, page ) of
                            ( UiLocales, SettingsRoutes.LocalesListPage ) ->
                                gettext "Application Translations" l

                            ( MailTemplateLocales, SettingsRoutes.LocalesListPage ) ->
                                gettext "Email Translations" l

                            ( _, SettingsRoutes.LocalesUploadPage ) ->
                                gettext "Upload translation" l

                            ( _, SettingsRoutes.LocalesRegistryPage ) ->
                                gettext "Import from Registry" l

        Routes.TenantsRoute tenantsRoute ->
            case tenantsRoute of
                TenantsRoutes.IndexRoute _ _ _ ->
                    "Tenants"

                TenantsRoutes.CreateRoute ->
                    "Create Tenant"

                TenantsRoutes.DetailRoute _ ->
                    "Tenant"

        Routes.UsersRoute usersRoute ->
            case usersRoute of
                UsersRoutes.CreateRoute ->
                    gettext "Create User" l

                UsersRoutes.IndexRoute _ _ ->
                    gettext "Users" l

                UsersRoutes.EditRoute _ editRoute ->
                    within (gettext "User Settings" l) <|
                        case editRoute of
                            UserEditRoutes.Profile ->
                                gettext "Profile" l

                            UserEditRoutes.Password ->
                                gettext "Password" l

                            UserEditRoutes.ConnectedAccounts ->
                                gettext "Connected Accounts" l

                            UserEditRoutes.Language ->
                                gettext "Language" l

                            UserEditRoutes.Tours ->
                                gettext "Tours" l

                            UserEditRoutes.ApiKeys ->
                                gettext "API Keys" l

                            UserEditRoutes.ActiveSessions ->
                                gettext "Active Sessions" l

                            UserEditRoutes.SubmissionSettings ->
                                gettext "Submission Settings" l

                            UserEditRoutes.PluginSettings _ ->
                                gettext "Plugin settings" l

        Routes.NotAllowedRoute ->
            gettext "Not Allowed" l

        Routes.NotFoundRoute ->
            gettext "Not Found" l


misconfigured : AppState -> Document msg
misconfigured appState =
    { title = gettext "Configuration Error" appState.locale
    , body = [ Page.misconfigured appState ]
    }


{-| A signed-in page inside the application frame.
-}
app : AppState -> AppConfig msg -> Html msg -> Document msg
app appState config content =
    let
        lookAndFeel =
            appState.config.lookAndFeel

        ( name, role, avatar ) =
            case appState.config.user of
                Just user ->
                    ( User.fullName user
                    , Role.localizedName appState.locale user.organizationRole
                    , UserAvatar.user
                        { firstName = user.firstName
                        , lastName = user.lastName
                        , imageUrl = user.imageUrl
                        , gravatarHash = UserAvatar.emailHash user.email
                        }
                    )

                Nothing ->
                    ( "", "", UserAvatar.item "" Nothing )

        html =
            AppFrame.view appState
                { brand =
                    { title = LookAndFeelConfig.getAppTitle lookAndFeel
                    , logoUrl = Just (LookAndFeelConfig.getLogoUrl lookAndFeel)
                    , homeUrl = Routes.toUrl appState Routes.appHome
                    }
                , context = config.context
                , items = config.menu
                , customLinks =
                    if config.customMenuLinks then
                        List.map
                            (\link -> { label = link.title, url = link.url, icon = link.icon, newWindow = link.newWindow })
                            appState.config.dashboardAndMenu.customMenuLinks

                    else
                        []
                , user = { name = name, role = role, avatar = avatar }
                , userMenu = userMenu appState config
                , mode = appState.uiMode
                , setMode = config.setMode
                , footerNotice = config.footerNotice
                , footerActions = config.footerActions
                , sidePanel = config.sidePanel
                , toMsg = config.frameMsg
                , content = [ content ]
                , overlays = config.overlays
                }
                config.frame
    in
    { title = documentTitle appState config.title
    , body = [ html ]
    }


{-| The frame when someone is signed in, the public frame when not: a public
knowledge model, a project shared by its link, or a page that was not found, can
be opened by anyone. Not signed in, the page is given the window under the public
header as the shell would give it its region (`PublicFrame.region`), so it
behaves the same either way.
-}
mixedApp : AppState -> AppConfig msg -> Html msg -> Document msg
mixedApp appState config content =
    if Maybe.isNothing appState.config.user then
        -- The page as it is in the application — a detail page with its header
        -- kept and its body, or a flush tab's own regions, scrolling — under
        -- the public header instead of beside the navigation.
        { title = documentTitle appState config.title
        , body =
            [ PublicFrame.region appState
                { brand = PublicLayout.brand appState
                , links = PublicLayout.links appState (Just config.currentUrl)
                , content = [ content ]
                , overlays = config.overlays
                }
            ]
        }

    else
        app appState config content


{-| A public page, which has drawn its own frame (`publicPanel`, `publicApp`).
-}
public : AppState -> { title : String, overlays : List (Html msg) } -> Html msg -> Document msg
public appState config content =
    { title = documentTitle appState config.title
    , body = content :: config.overlays
    }


userMenu : AppState -> AppConfig msg -> { items : List (Menu.Item msg), moreItems : List (Menu.Item msg) }
userMenu appState config =
    let
        changeLanguageUrl =
            Routes.toUrl appState Routes.usersEditLanguageCurrent

        assignedComments =
            if not config.assignedComments then
                []

            else
                [ Menu.withIcon (Icon.solid "fa-check-to-slot") (Menu.linkItem (gettext "Assigned comments" appState.locale) (Routes.toUrl appState Routes.commentsIndex)) |> Menu.withTestId "user-menu_assigned-comments" ]

        newsItem =
            if Maybe.isJust appState.newsUrl && Feature.newsModal appState then
                [ Menu.withIcon (Icon.regular "fa-lightbulb") (Menu.item (gettext "What's new" appState.locale) config.openNewsMsg) |> Menu.withTestId "user-menu_news" ]

            else
                []
    in
    { items =
        (Menu.withIcon (Icon.solid "fa-user-pen") (Menu.linkItem (gettext "User settings" appState.locale) (Routes.toUrl appState Routes.usersEditCurrent))
            |> Menu.withTestId "user-menu_settings"
        )
            :: assignedComments
            ++ [ Menu.withIcon (Icon.solid "fa-language") (Menu.linkItem (gettext "Change language" appState.locale) changeLanguageUrl)
                    |> Menu.withTestId "user-menu_language"

               -- The palette is opened from the keyboard; this is where it can be
               -- found, and where the keys are learnt.
               , Menu.item (gettext "Go to…" appState.locale) config.openCommandPaletteMsg
                    |> Menu.withIcon (Icon.solid "fa-magnifying-glass")
                    |> Menu.withTestId "user-menu_command-palette"
                    |> Menu.withShortcut
                        (if appState.navigator.isMac then
                            { label = "⌘K", keys = "Meta+K" }

                         else
                            { label = "Ctrl+K", keys = "Control+K" }
                        )
               ]
    , moreItems =
        (Menu.withIcon (Icon.solid "fa-info") (Menu.item (gettext "About" appState.locale) config.openAboutMsg)
            |> Menu.withTestId "user-menu_about"
        )
            :: newsItem
            ++ [ Menu.withIcon (Icon.solid "fa-triangle-exclamation") (Menu.item (gettext "Report issue" appState.locale) config.openReportIssueMsg)
                    |> Menu.withTestId "user-menu_report-issue"
               , Menu.withIcon (Icon.solid "fa-right-from-bracket") (Menu.item (gettext "Log out" appState.locale) config.logoutMsg)
                    |> Menu.withTestId "user-menu_logout"
               ]
    }



-- MENU


{-| One entry of a group: `soloTitle` names it when it is the only one the
reader may see, and the group is then drawn as that one link — "Knowledge
Models" rather than "List", which means nothing on its own.
-}
type alias MenuGroupItem =
    { id : String
    , title : String
    , soloTitle : Maybe String
    , url : String
    , active : Bool
    , visible : Bool
    }


{-| A link in the navigation, when the reader may see it. The ids are the
wizard's (`menu_<id>`), which the tours and the end-to-end tests look for.
-}
menuItem : { id : String, icon : String, label : String, url : String, active : Bool, visible : Bool } -> List (MenuEntry msg)
menuItem config =
    if config.visible then
        [ AppFrame.item (Icon.solid config.icon) { label = config.label, url = config.url, active = config.active }
            |> AppFrame.withId ("menu_" ++ config.id)
            |> AppFrame.withTestId ("menu_" ++ config.id)
        ]

    else
        []


{-| A group of links. A group with no visible children is left out, and a
group with one is drawn as that one link under the group's icon.
-}
menuGroup : { id : String, icon : String, label : String, active : Bool } -> List MenuGroupItem -> List (MenuEntry msg)
menuGroup config items =
    case List.filter .visible items of
        [] ->
            []

        [ single ] ->
            [ AppFrame.item (Icon.solid config.icon)
                { label = Maybe.withDefault single.title single.soloTitle
                , url = single.url
                , active = single.active
                }
                |> AppFrame.withId ("menu_" ++ single.id)
                |> AppFrame.withTestId ("menu_" ++ single.id)
            ]

        (first :: _) as visibleItems ->
            [ AppFrame.group (Icon.solid config.icon)
                { label = config.label
                , url = first.url
                , active = config.active
                }
                (List.map
                    (\groupItem ->
                        AppFrame.child
                            { label = groupItem.title
                            , url = groupItem.url
                            , active = groupItem.active
                            }
                            |> AppFrame.withTestId ("menu_" ++ groupItem.id)
                    )
                    visibleItems
                )
                |> AppFrame.withId ("menu_" ++ config.id)
                |> AppFrame.withTestId ("menu_" ++ config.id)
            ]
