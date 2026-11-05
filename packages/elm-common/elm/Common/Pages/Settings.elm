module Common.Pages.Settings exposing
    ( Active(..)
    , Model
    , Mount
    , Msg
    , Section
    , defaultMount
    , entityName
    , frame
    , init
    , initialModel
    , isGuarded
    , navigation
    , subscriptions
    , update
    , view
    )

{-| Settings: the feature router.

Every section is drawn inside `Common.Components.SettingsPage.layout`, with one
menu and its gating: plugins only when there are any, and the registry only where the registry feature is.

A client adds sections of its own to the menu through
`Common.Data.Extensions` and draws them in the same layout with `frame`.

Where the settings are mounted says what is there (`Mount`): which sections,
under which name, and which permissions a role can be given.

-}

import Common.Api.Models.RolePermission exposing (PermissionGroup)
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.Extensions as Extensions
import Common.Data.Feature as Feature
import Common.Data.LocaleKind exposing (LocaleKind(..))
import Common.Data.UpdateConfig as UpdateConfig exposing (UpdateConfig)
import Common.Data.WizardRolePermission as WizardRolePermission
import Common.Pages.Settings.Authentication as Authentication
import Common.Pages.Settings.DashboardAndMenu as DashboardAndMenu
import Common.Pages.Settings.Features as Features
import Common.Pages.Settings.Locales.Index as LocalesIndex
import Common.Pages.Settings.Locales.Registry as LocalesRegistry
import Common.Pages.Settings.Locales.Upload as LocalesUpload
import Common.Pages.Settings.LoginScreen as LoginScreen
import Common.Pages.Settings.LookAndFeel as LookAndFeel
import Common.Pages.Settings.OpenId as OpenId
import Common.Pages.Settings.OpenIdCreate as OpenIdCreate
import Common.Pages.Settings.OpenIdDetail as OpenIdDetail
import Common.Pages.Settings.PluginSettings as PluginSettings
import Common.Pages.Settings.Plugins as Plugins
import Common.Pages.Settings.Projects as Projects
import Common.Pages.Settings.Registry as Registry
import Common.Pages.Settings.RoleCreate as RoleCreate
import Common.Pages.Settings.RoleDetail as RoleDetail
import Common.Pages.Settings.Roles as Roles
import Common.Pages.Settings.Routes as SettingsRoutes exposing (LocalesPage(..), Route(..))
import Common.Pages.Settings.Submission as Submission
import Common.Pages.Settings.Support as Support
import Common.Pages.Settings.Usage as Usage
import Common.Pages.Settings.Users as Users
import Common.Routes as Routes
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import Random
import Uuid


{-| What differs where the settings are mounted.

  - `title` names the menu.
  - `sections` says which sections are there and may be opened.
  - `permissionGroups` are the permissions a role is offered in the role editor,
    `permissionNote` something to say above them, and `permissionReach` a
    badge for the switches of the permissions that hold in more than one place
    — how far they reach.
  - `extras` says whether the client's own sections
    (`Common.Data.Extensions`) are in the menu.

-}
type alias Mount =
    { title : String
    , sections : SettingsRoutes.Route -> Bool
    , permissionGroups : List PermissionGroup
    , permissionNote : Maybe String
    , permissionReach : Maybe String
    , extras : Bool
    }


{-| Every section the user may open, and every permission there is here.
-}
defaultMount : AppState -> Mount
defaultMount appState =
    { title = gettext "Settings" appState.locale
    , sections = \route -> SettingsRoutes.isAllowed route appState
    , permissionGroups = WizardRolePermission.allGroups
    , permissionNote = Nothing
    , permissionReach = Nothing
    , extras = True
    }


type alias Model =
    { usersModel : Users.Model
    , authenticationModel : Authentication.Model
    , openIdModel : OpenId.Model
    , openIdCreateModel : OpenIdCreate.Model
    , openIdDetailModel : OpenIdDetail.Model
    , rolesModel : Roles.Model
    , roleCreateModel : RoleCreate.Model
    , roleDetailModel : RoleDetail.Model
    , supportModel : Support.Model
    , featuresModel : Features.Model
    , pluginsModel : Plugins.Model
    , pluginSettingsModel : PluginSettings.Model
    , loginScreenModel : LoginScreen.Model
    , dashboardAndMenuModel : DashboardAndMenu.Model
    , lookAndFeelModel : LookAndFeel.Model
    , registryModel : Registry.Model
    , projectsModel : Projects.Model
    , submissionModel : Submission.Model
    , usageModel : Usage.Model
    , localesIndexModel : LocalesIndex.Model
    , localesUploadModel : LocalesUpload.Model
    , localesRegistryModel : LocalesRegistry.Model
    }


type Msg
    = UsersMsg Users.Msg
    | AuthenticationMsg Authentication.Msg
    | OpenIdMsg OpenId.Msg
    | OpenIdCreateMsg OpenIdCreate.Msg
    | OpenIdDetailMsg OpenIdDetail.Msg
    | RolesMsg Roles.Msg
    | RoleCreateMsg RoleCreate.Msg
    | RoleDetailMsg RoleDetail.Msg
    | SupportMsg Support.Msg
    | FeaturesMsg Features.Msg
    | PluginsMsg Plugins.Msg
    | PluginSettingsMsg PluginSettings.Msg
    | LoginScreenMsg LoginScreen.Msg
    | DashboardAndMenuMsg DashboardAndMenu.Msg
    | LookAndFeelMsg LookAndFeel.Msg
    | RegistryMsg Registry.Msg
    | ProjectsMsg Projects.Msg
    | SubmissionMsg Submission.Msg
    | UsageMsg Usage.Msg
    | LocalesIndexMsg LocalesIndex.Msg
    | LocalesUploadMsg LocalesUpload.Msg
    | LocalesRegistryMsg LocalesRegistry.Msg


initialModel : AppState -> Model
initialModel appState =
    { usersModel = Users.initialModel
    , authenticationModel = Authentication.initialModel
    , openIdModel = OpenId.initialModel
    , openIdCreateModel = OpenIdCreate.initialModel appState
    , openIdDetailModel = OpenIdDetail.initialModel appState Uuid.nil
    , rolesModel = Roles.initialModel
    , roleCreateModel = RoleCreate.initialModel
    , roleDetailModel = RoleDetail.initialModel Uuid.nil
    , supportModel = Support.initialModel
    , featuresModel = Features.initialModel
    , pluginsModel = Plugins.initialModel
    , pluginSettingsModel = PluginSettings.initialModel Uuid.nil
    , loginScreenModel = LoginScreen.initialModel
    , dashboardAndMenuModel = DashboardAndMenu.initialModel
    , lookAndFeelModel = LookAndFeel.initialModel
    , registryModel = Registry.initialModel
    , projectsModel = Projects.initialModel appState
    , submissionModel = Submission.initialModel
    , usageModel = Usage.initialModel
    , localesIndexModel = LocalesIndex.initialModel UiLocales
    , localesUploadModel = LocalesUpload.initialModel UiLocales
    , localesRegistryModel = LocalesRegistry.initialModel UiLocales
    }


init : AppState -> Model -> SettingsRoutes.Route -> ( Model, Cmd Msg )
init appState model route =
    case route of
        UsersRoute ->
            Tuple.mapBoth (\m -> { model | usersModel = m }) (Cmd.map UsersMsg) (Users.init appState)

        AuthenticationRoute ->
            Tuple.mapBoth (\m -> { model | authenticationModel = m }) (Cmd.map AuthenticationMsg) (Authentication.init appState)

        OpenIdRoute ->
            Tuple.mapBoth (\m -> { model | openIdModel = m }) (Cmd.map OpenIdMsg) (OpenId.init appState)

        OpenIdCreateRoute ->
            Tuple.mapBoth (\m -> { model | openIdCreateModel = m }) (Cmd.map OpenIdCreateMsg) (OpenIdCreate.init appState)

        OpenIdDetailRoute uuid ->
            Tuple.mapBoth (\m -> { model | openIdDetailModel = m }) (Cmd.map OpenIdDetailMsg) (OpenIdDetail.init appState uuid)

        RolesRoute ->
            Tuple.mapBoth (\m -> { model | rolesModel = m }) (Cmd.map RolesMsg) (Roles.init appState)

        RoleCreateRoute ->
            Tuple.mapBoth (\m -> { model | roleCreateModel = m }) (Cmd.map RoleCreateMsg) RoleCreate.init

        RoleDetailRoute uuid ->
            Tuple.mapBoth (\m -> { model | roleDetailModel = m }) (Cmd.map RoleDetailMsg) (RoleDetail.init appState uuid)

        SupportRoute ->
            Tuple.mapBoth (\m -> { model | supportModel = m }) (Cmd.map SupportMsg) (Support.init appState)

        FeaturesRoute ->
            Tuple.mapBoth (\m -> { model | featuresModel = m }) (Cmd.map FeaturesMsg) (Features.init appState)

        PluginsRoute ->
            Tuple.mapBoth (\m -> { model | pluginsModel = m }) (Cmd.map PluginsMsg) (Plugins.init appState)

        PluginSettingsRoute pluginUuid ->
            Tuple.mapBoth (\m -> { model | pluginSettingsModel = m }) (Cmd.map PluginSettingsMsg) (PluginSettings.init appState pluginUuid)

        LoginScreenRoute ->
            Tuple.mapBoth (\m -> { model | loginScreenModel = m }) (Cmd.map LoginScreenMsg) (LoginScreen.init appState)

        DashboardAndMenuRoute ->
            Tuple.mapBoth (\m -> { model | dashboardAndMenuModel = m }) (Cmd.map DashboardAndMenuMsg) (DashboardAndMenu.init appState)

        LookAndFeelRoute ->
            Tuple.mapBoth (\m -> { model | lookAndFeelModel = m }) (Cmd.map LookAndFeelMsg) (LookAndFeel.init appState)

        RegistryRoute ->
            Tuple.mapBoth (\m -> { model | registryModel = m }) (Cmd.map RegistryMsg) (Registry.init appState)

        ProjectsRoute ->
            Tuple.mapBoth (\m -> { model | projectsModel = m }) (Cmd.map ProjectsMsg) (Projects.init appState)

        SubmissionRoute ->
            Tuple.mapBoth (\m -> { model | submissionModel = m }) (Cmd.map SubmissionMsg) (Submission.init appState)

        UsageRoute ->
            Tuple.mapBoth (\m -> { model | usageModel = m }) (Cmd.map UsageMsg) (Usage.init appState)

        LocalesRoute kind LocalesListPage ->
            Tuple.mapBoth (\m -> { model | localesIndexModel = m }) (Cmd.map LocalesIndexMsg) (LocalesIndex.init appState kind)

        LocalesRoute kind LocalesUploadPage ->
            Tuple.mapBoth (\m -> { model | localesUploadModel = m }) (Cmd.map LocalesUploadMsg) (LocalesUpload.init appState kind)

        LocalesRoute kind LocalesRegistryPage ->
            Tuple.mapBoth (\m -> { model | localesRegistryModel = m }) (Cmd.map LocalesRegistryMsg) (LocalesRegistry.init appState kind)


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Random.Seed, Model, Cmd msg )
update cfg appState msg model =
    let
        withSeed ( newModel, cmd ) =
            ( appState.seed, newModel, cmd )
    in
    withSeed <|
        case msg of
            UsersMsg subMsg ->
                Tuple.mapFirst (\m -> { model | usersModel = m }) <|
                    Users.update (UpdateConfig.map UsersMsg cfg) appState subMsg model.usersModel

            AuthenticationMsg subMsg ->
                Tuple.mapFirst (\m -> { model | authenticationModel = m }) <|
                    Authentication.update (UpdateConfig.map AuthenticationMsg cfg) appState subMsg model.authenticationModel

            OpenIdMsg subMsg ->
                Tuple.mapFirst (\m -> { model | openIdModel = m }) <|
                    OpenId.update (UpdateConfig.map OpenIdMsg cfg) appState subMsg model.openIdModel

            OpenIdCreateMsg subMsg ->
                Tuple.mapFirst (\m -> { model | openIdCreateModel = m }) <|
                    OpenIdCreate.update (UpdateConfig.map OpenIdCreateMsg cfg) appState subMsg model.openIdCreateModel

            OpenIdDetailMsg subMsg ->
                Tuple.mapFirst (\m -> { model | openIdDetailModel = m }) <|
                    OpenIdDetail.update (UpdateConfig.map OpenIdDetailMsg cfg) appState subMsg model.openIdDetailModel

            RolesMsg subMsg ->
                Tuple.mapFirst (\m -> { model | rolesModel = m }) <|
                    Roles.update (UpdateConfig.map RolesMsg cfg) appState subMsg model.rolesModel

            RoleCreateMsg subMsg ->
                Tuple.mapFirst (\m -> { model | roleCreateModel = m }) <|
                    RoleCreate.update (UpdateConfig.map RoleCreateMsg cfg) appState subMsg model.roleCreateModel

            RoleDetailMsg subMsg ->
                Tuple.mapFirst (\m -> { model | roleDetailModel = m }) <|
                    RoleDetail.update (UpdateConfig.map RoleDetailMsg cfg) appState subMsg model.roleDetailModel

            SupportMsg subMsg ->
                Tuple.mapFirst (\m -> { model | supportModel = m }) <|
                    Support.update (UpdateConfig.map SupportMsg cfg) appState subMsg model.supportModel

            FeaturesMsg subMsg ->
                Tuple.mapFirst (\m -> { model | featuresModel = m }) <|
                    Features.update (UpdateConfig.map FeaturesMsg cfg) appState subMsg model.featuresModel

            PluginsMsg subMsg ->
                Tuple.mapFirst (\m -> { model | pluginsModel = m }) <|
                    Plugins.update (UpdateConfig.map PluginsMsg cfg) appState subMsg model.pluginsModel

            PluginSettingsMsg subMsg ->
                Tuple.mapBoth (\m -> { model | pluginSettingsModel = m }) (Cmd.map (cfg.wrapMsg << PluginSettingsMsg)) <|
                    PluginSettings.update appState subMsg model.pluginSettingsModel

            LoginScreenMsg subMsg ->
                Tuple.mapFirst (\m -> { model | loginScreenModel = m }) <|
                    LoginScreen.update (UpdateConfig.map LoginScreenMsg cfg) appState subMsg model.loginScreenModel

            DashboardAndMenuMsg subMsg ->
                Tuple.mapFirst (\m -> { model | dashboardAndMenuModel = m }) <|
                    DashboardAndMenu.update (UpdateConfig.map DashboardAndMenuMsg cfg) appState subMsg model.dashboardAndMenuModel

            LookAndFeelMsg subMsg ->
                Tuple.mapFirst (\m -> { model | lookAndFeelModel = m }) <|
                    LookAndFeel.update (UpdateConfig.map LookAndFeelMsg cfg) appState subMsg model.lookAndFeelModel

            RegistryMsg subMsg ->
                Tuple.mapFirst (\m -> { model | registryModel = m }) <|
                    Registry.update (UpdateConfig.map RegistryMsg cfg) appState subMsg model.registryModel

            ProjectsMsg subMsg ->
                Tuple.mapFirst (\m -> { model | projectsModel = m }) <|
                    Projects.update (UpdateConfig.map ProjectsMsg cfg) appState subMsg model.projectsModel

            SubmissionMsg subMsg ->
                Tuple.mapFirst (\m -> { model | submissionModel = m }) <|
                    Submission.update (UpdateConfig.map SubmissionMsg cfg) appState subMsg model.submissionModel

            UsageMsg subMsg ->
                Tuple.mapFirst (\m -> { model | usageModel = m }) <|
                    Usage.update (UpdateConfig.map UsageMsg cfg) appState subMsg model.usageModel

            LocalesIndexMsg subMsg ->
                Tuple.mapFirst (\m -> { model | localesIndexModel = m }) <|
                    LocalesIndex.update (UpdateConfig.map LocalesIndexMsg cfg) appState subMsg model.localesIndexModel

            LocalesUploadMsg subMsg ->
                Tuple.mapFirst (\m -> { model | localesUploadModel = m }) <|
                    LocalesUpload.update (UpdateConfig.map LocalesUploadMsg cfg) appState subMsg model.localesUploadModel

            LocalesRegistryMsg subMsg ->
                Tuple.mapFirst (\m -> { model | localesRegistryModel = m }) <|
                    LocalesRegistry.update (UpdateConfig.map LocalesRegistryMsg cfg) appState subMsg model.localesRegistryModel


subscriptions : AppState -> SettingsRoutes.Route -> Model -> Sub Msg
subscriptions appState route model =
    case route of
        UsersRoute ->
            Sub.map UsersMsg (Users.subscriptions appState model.usersModel)

        AuthenticationRoute ->
            Sub.map AuthenticationMsg (Authentication.subscriptions appState model.authenticationModel)

        OpenIdCreateRoute ->
            Sub.map OpenIdCreateMsg (OpenIdCreate.subscriptions appState model.openIdCreateModel)

        OpenIdDetailRoute _ ->
            Sub.map OpenIdDetailMsg (OpenIdDetail.subscriptions appState model.openIdDetailModel)

        RoleCreateRoute ->
            Sub.map RoleCreateMsg (RoleCreate.subscriptions appState model.roleCreateModel)

        RoleDetailRoute _ ->
            Sub.map RoleDetailMsg (RoleDetail.subscriptions appState model.roleDetailModel)

        SupportRoute ->
            Sub.map SupportMsg (Support.subscriptions appState model.supportModel)

        FeaturesRoute ->
            Sub.map FeaturesMsg (Features.subscriptions appState model.featuresModel)

        PluginsRoute ->
            Sub.map PluginsMsg (Plugins.subscriptions appState model.pluginsModel)

        PluginSettingsRoute _ ->
            Sub.map PluginSettingsMsg (PluginSettings.subscriptions appState model.pluginSettingsModel)

        LoginScreenRoute ->
            Sub.map LoginScreenMsg (LoginScreen.subscriptions appState model.loginScreenModel)

        DashboardAndMenuRoute ->
            Sub.map DashboardAndMenuMsg (DashboardAndMenu.subscriptions appState model.dashboardAndMenuModel)

        LookAndFeelRoute ->
            Sub.map LookAndFeelMsg (LookAndFeel.subscriptions appState model.lookAndFeelModel)

        RegistryRoute ->
            Sub.map RegistryMsg (Registry.subscriptions appState model.registryModel)

        ProjectsRoute ->
            Sub.map ProjectsMsg (Projects.subscriptions appState model.projectsModel)

        SubmissionRoute ->
            Sub.map SubmissionMsg (Submission.subscriptions appState model.submissionModel)

        _ ->
            Sub.none



-- VIEW


type alias Section msg =
    { body : List (Html msg), footer : Maybe (Html msg) }


view : Mount -> AppState -> SettingsRoutes.Route -> Model -> Html Msg
view mount appState route model =
    let
        section =
            case route of
                UsersRoute ->
                    mapSection UsersMsg (Users.view appState model.usersModel)

                AuthenticationRoute ->
                    mapSection AuthenticationMsg (Authentication.view appState model.authenticationModel)

                OpenIdRoute ->
                    mapSection OpenIdMsg (OpenId.view appState model.openIdModel)

                OpenIdCreateRoute ->
                    mapSection OpenIdCreateMsg (OpenIdCreate.view appState model.openIdCreateModel)

                OpenIdDetailRoute _ ->
                    mapSection OpenIdDetailMsg (OpenIdDetail.view appState model.openIdDetailModel)

                RolesRoute ->
                    mapSection RolesMsg (Roles.view appState model.rolesModel)

                RoleCreateRoute ->
                    mapSection RoleCreateMsg (RoleCreate.view { groups = mount.permissionGroups, note = mount.permissionNote, reach = mount.permissionReach } appState model.roleCreateModel)

                RoleDetailRoute _ ->
                    mapSection RoleDetailMsg (RoleDetail.view { groups = mount.permissionGroups, note = mount.permissionNote, reach = mount.permissionReach } appState model.roleDetailModel)

                SupportRoute ->
                    mapSection SupportMsg (Support.view appState model.supportModel)

                FeaturesRoute ->
                    mapSection FeaturesMsg (Features.view appState model.featuresModel)

                PluginsRoute ->
                    mapSection PluginsMsg (Plugins.view appState model.pluginsModel)

                PluginSettingsRoute _ ->
                    mapSection PluginSettingsMsg (PluginSettings.view appState model.pluginSettingsModel)

                LoginScreenRoute ->
                    mapSection LoginScreenMsg (LoginScreen.view appState model.loginScreenModel)

                DashboardAndMenuRoute ->
                    mapSection DashboardAndMenuMsg (DashboardAndMenu.view appState model.dashboardAndMenuModel)

                LookAndFeelRoute ->
                    mapSection LookAndFeelMsg (LookAndFeel.view appState model.lookAndFeelModel)

                RegistryRoute ->
                    mapSection RegistryMsg (Registry.view appState model.registryModel)

                ProjectsRoute ->
                    mapSection ProjectsMsg (Projects.view appState model.projectsModel)

                SubmissionRoute ->
                    mapSection SubmissionMsg (Submission.view appState model.submissionModel)

                UsageRoute ->
                    mapSection UsageMsg (Usage.view appState model.usageModel)

                LocalesRoute kind LocalesListPage ->
                    mapSection LocalesIndexMsg (LocalesIndex.view appState { registry = mount.sections (LocalesRoute kind LocalesRegistryPage) } model.localesIndexModel)

                LocalesRoute _ LocalesUploadPage ->
                    mapSection LocalesUploadMsg (LocalesUpload.view appState model.localesUploadModel)

                LocalesRoute _ LocalesRegistryPage ->
                    mapSection LocalesRegistryMsg (LocalesRegistry.view appState model.localesRegistryModel)
    in
    frame mount appState (SharedSection route) section


{-| Which section of the menu is the one shown: one of these pages, or one a
client added (`Common.Data.Extensions`), by its URL.
-}
type Active
    = SharedSection SettingsRoutes.Route
    | ExtraSection String


{-| A section inside the settings layout, with the menu marking `active`: how a
client shows the sections it adds, so they sit in the same page as these.
-}
frame : Mount -> AppState -> Active -> Section msg -> Html msg
frame mount appState active section =
    SettingsPage.layout
        { title = mount.title
        , groups = navigationAt mount appState active
        , body = section.body
        , footer = section.footer
        }


mapSection : (subMsg -> Msg) -> Section subMsg -> Section Msg
mapSection toMsg section =
    { body = List.map (Html.map toMsg) section.body
    , footer = Maybe.map (Html.map toMsg) section.footer
    }


navigation : Mount -> AppState -> SettingsRoutes.Route -> List ( Maybe String, List { label : String, url : String, active : Bool } )
navigation mount appState currentRoute =
    navigationAt mount appState (SharedSection currentRoute)


navigationAt : Mount -> AppState -> Active -> List ( Maybe String, List { label : String, url : String, active : Bool } )
navigationAt mount appState active =
    let
        -- Only the sections that are there are in the menu.
        sectionItems =
            List.filter (\( route, _, _ ) -> mount.sections route) >> List.map item

        item ( route, label, isActive ) =
            { label = label
            , url = Routes.toUrl appState (Routes.SettingsRoute route)
            , active =
                case active of
                    SharedSection currentRoute ->
                        isActive currentRoute

                    ExtraSection _ ->
                        False
            }

        -- The client's sections, each after the one it names. They are
        -- inserted last to first (`foldr`), so two after the same section, or
        -- two first in a group, keep the order they were given in.
        withExtras group groupItems =
            if mount.extras then
                appState.extensions.settings
                    |> List.filter (\entry -> entry.group == group)
                    |> List.foldr insertExtra groupItems

            else
                groupItems

        insertExtra entry items =
            let
                extra =
                    { label = entry.label, url = entry.url, active = active == ExtraSection entry.url }
            in
            case entry.after of
                Nothing ->
                    extra :: items

                Just afterUrl ->
                    case List.findIndex (\i -> i.url == afterUrl) items of
                        Just index ->
                            List.take (index + 1) items ++ extra :: List.drop (index + 1) items

                        Nothing ->
                            items ++ [ extra ]

        is =
            (==)

        -- A create or detail page is within its list's section, as the
        -- plugin settings are within Plugins.
        isOpenId route =
            case route of
                OpenIdRoute ->
                    True

                OpenIdCreateRoute ->
                    True

                OpenIdDetailRoute _ ->
                    True

                _ ->
                    False

        isRoles route =
            case route of
                RolesRoute ->
                    True

                RoleCreateRoute ->
                    True

                RoleDetailRoute _ ->
                    True

                _ ->
                    False

        -- The upload and the registry are within their translations' section.
        isLocales kind route =
            case route of
                LocalesRoute routeKind _ ->
                    routeKind == kind

                _ ->
                    False

        usersAndAccessLinks =
            [ ( AuthenticationRoute, gettext "Authentication" appState.locale, is AuthenticationRoute )
            , ( OpenIdRoute, gettext "OpenID" appState.locale, isOpenId )
            , ( RolesRoute, gettext "Roles" appState.locale, isRoles )
            , ( UsersRoute, gettext "Users" appState.locale, is UsersRoute )
            ]

        projectsLinks =
            [ ( ProjectsRoute, gettext "Projects" appState.locale, is ProjectsRoute )
            , ( SubmissionRoute, gettext "Document Submission" appState.locale, is SubmissionRoute )
            ]

        appearanceLinks =
            [ ( LookAndFeelRoute, gettext "Look & Feel" appState.locale, is LookAndFeelRoute )
            , ( DashboardAndMenuRoute, gettext "Dashboard & Menu" appState.locale, is DashboardAndMenuRoute )
            , ( LoginScreenRoute, gettext "Login Screen" appState.locale, is LoginScreenRoute )
            , ( SupportRoute, gettext "Support" appState.locale, is SupportRoute )
            ]

        featuresAndIntegrationsLinks =
            ( FeaturesRoute, gettext "Features" appState.locale, is FeaturesRoute )
                :: (if AppState.anyPluginsAvailable appState then
                        [ ( PluginsRoute, gettext "Plugins" appState.locale, SettingsRoutes.isPluginsSubroute ) ]

                    else
                        []
                   )
                ++ (if Feature.registry appState then
                        [ ( RegistryRoute, gettext "Registry" appState.locale, is RegistryRoute ) ]

                    else
                        []
                   )

        translationsLinks =
            [ ( LocalesRoute UiLocales LocalesListPage, gettext "Application" appState.locale, isLocales UiLocales )
            , ( LocalesRoute MailTemplateLocales LocalesListPage, gettext "Emails" appState.locale, isLocales MailTemplateLocales )
            ]

        planAndUsageLinks =
            [ ( UsageRoute, gettext "Usage" appState.locale, is UsageRoute ) ]
    in
    [ ( Just (gettext "Users & Access" appState.locale), withExtras Extensions.UsersAndAccessSettings (sectionItems usersAndAccessLinks) )
    , ( Just (gettext "Projects" appState.locale), withExtras Extensions.ProjectsSettings (sectionItems projectsLinks) )
    , ( Just (gettext "Appearance" appState.locale), withExtras Extensions.AppearanceSettings (sectionItems appearanceLinks) )
    , ( Just (gettext "Features & Integrations" appState.locale), withExtras Extensions.FeaturesAndIntegrationsSettings (sectionItems featuresAndIntegrationsLinks) )
    , ( Just (gettext "Translations" appState.locale), withExtras Extensions.TranslationsSettings (sectionItems translationsLinks) )
    , ( Just (gettext "Plan & Usage" appState.locale), withExtras Extensions.PlanAndUsageSettings (sectionItems planAndUsageLinks) )
    ]


isGuarded : AppState -> SettingsRoutes.Route -> Model -> Maybe String
isGuarded appState route model =
    case route of
        UsersRoute ->
            Users.isGuarded appState model.usersModel

        AuthenticationRoute ->
            Authentication.isGuarded appState model.authenticationModel

        OpenIdDetailRoute _ ->
            OpenIdDetail.isGuarded appState model.openIdDetailModel

        RoleDetailRoute _ ->
            RoleDetail.isGuarded appState model.roleDetailModel

        SupportRoute ->
            Support.isGuarded appState model.supportModel

        FeaturesRoute ->
            Features.isGuarded appState model.featuresModel

        PluginsRoute ->
            Plugins.isGuarded appState model.pluginsModel

        PluginSettingsRoute _ ->
            PluginSettings.isGuarded appState model.pluginSettingsModel

        LoginScreenRoute ->
            LoginScreen.isGuarded appState model.loginScreenModel

        DashboardAndMenuRoute ->
            DashboardAndMenu.isGuarded appState model.dashboardAndMenuModel

        LookAndFeelRoute ->
            LookAndFeel.isGuarded appState model.lookAndFeelModel

        RegistryRoute ->
            Registry.isGuarded appState model.registryModel

        ProjectsRoute ->
            Projects.isGuarded appState model.projectsModel

        SubmissionRoute ->
            Submission.isGuarded appState model.submissionModel

        _ ->
            Nothing


{-| The name of what the page at a route is about, for the window's title: a
detail page's entity, once it has arrived.
-}
entityName : Gettext.Locale -> Model -> SettingsRoutes.Route -> Maybe String
entityName locale model route =
    case route of
        OpenIdDetailRoute _ ->
            OpenIdDetail.entityName model.openIdDetailModel

        RoleDetailRoute _ ->
            RoleDetail.entityName locale model.roleDetailModel

        _ ->
            Nothing
