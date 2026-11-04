module Common.Api.Models.BootstrapConfig exposing
    ( BootstrapConfig
    , Replaced
    , WorkspaceConfig
    , addTour
    , decoder
    , default
    , ownLookAndFeel
    , withDefaultAppTitle
    , withLook
    , withWorkspace
    , withoutWorkspace
    , workspaceDecoder
    )

{-| What the application is started with, in two parts.

The first (`decoder`) is there before anybody signs in: how to sign in, what
the login screen says, the look of the instance, whether documents can be
submitted, and who is signed in.

The second (`workspaceDecoder`) is what somebody signed in works with: the menu
links and announcements, how projects behave, where support is, whether
documents can be submitted, and which plugins are switched off. It is read
separately and laid over the first with `withWorkspace`; until it is, its
fields hold their defaults, or the first part's where it has them too.

-}

import Color exposing (Color)
import Common.Api.Models.BootstrapConfig.AuthenticationConfig as AuthenticationConfig exposing (AuthenticationConfig)
import Common.Api.Models.BootstrapConfig.CloudConfig as CloudConfig exposing (CloudConfig)
import Common.Api.Models.BootstrapConfig.FeaturesConfig as FeaturesConfig exposing (FeaturesConfig)
import Common.Api.Models.BootstrapConfig.LookAndFeelConfig as LookAndFeelConfig exposing (LookAndFeelConfig)
import Common.Api.Models.BootstrapConfig.PluginInfo as PluginInfo exposing (PluginInfo)
import Common.Api.Models.BootstrapConfig.PrivacyConfig as PrivacyConfig exposing (PrivacyConfig)
import Common.Api.Models.BootstrapConfig.ProjectConfig as ProjectConfig exposing (ProjectConfig)
import Common.Api.Models.BootstrapConfig.RegistryConfig as RegistryConfig exposing (RegistryConfig)
import Common.Api.Models.BootstrapConfig.SubmissionConfig as SubmissionConfig exposing (SubmissionConfig)
import Common.Api.Models.BootstrapConfig.UserConfig as UserConfig exposing (UserConfig)
import Common.Api.Models.Settings.DashboardAndMenuSettings as DashboardAndMenuSettings exposing (DashboardAndMenuSettings)
import Common.Api.Models.Settings.LoginScreenSettings as LoginScreenSettings exposing (LoginScreenSettings)
import Common.Api.Models.Settings.SupportSettings as SupportSettings exposing (SupportSettings)
import Common.Api.Models.Settings.UsersSettings as UsersSettings exposing (UsersSettings)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Color as D
import Json.Decode.Extensions as D
import Json.Decode.Pipeline as D
import Maybe.Extra as Maybe


type alias BootstrapConfig =
    { features : FeaturesConfig
    , authentication : AuthenticationConfig
    , loginScreen : LoginScreenSettings
    , registry : RegistryConfig
    , lookAndFeel : LookAndFeelConfig
    , users : UsersSettings
    , privacy : PrivacyConfig
    , pluginSettings : Dict String String
    , plugins : List PluginInfo
    , cloud : CloudConfig
    , tours : List String
    , user : Maybe UserConfig

    -- The second part.
    , dashboardAndMenu : DashboardAndMenuSettings
    , project : ProjectConfig
    , support : SupportSettings
    , submission : SubmissionConfig
    , disabledPlugins : List String

    -- What the second part replaced, to put back when it is taken away.
    , replaced : Maybe Replaced
    }


type alias Replaced =
    { lookAndFeel : LookAndFeelConfig
    , pluginSettings : Dict String String
    , dashboardAndMenu : DashboardAndMenuSettings
    , support : SupportSettings
    , submission : SubmissionConfig
    }


type alias WorkspaceConfig =
    { logoUrl : Maybe String
    , primaryColor : Maybe Color
    , dashboardAndMenu : DashboardAndMenuSettings
    , project : ProjectConfig
    , support : SupportSettings
    , submission : SubmissionConfig
    , disabledPlugins : List String
    , pluginSettings : Dict String String
    }


default : BootstrapConfig
default =
    { features = FeaturesConfig.default
    , authentication = AuthenticationConfig.default
    , loginScreen = LoginScreenSettings.default
    , registry = RegistryConfig.default
    , lookAndFeel = LookAndFeelConfig.default
    , users = UsersSettings.default
    , privacy = PrivacyConfig.default
    , pluginSettings = Dict.empty
    , plugins = []
    , cloud = CloudConfig.default
    , tours = []
    , user = Nothing
    , dashboardAndMenu = DashboardAndMenuSettings.default
    , project = ProjectConfig.default
    , support = SupportSettings.default
    , submission = SubmissionConfig.default
    , disabledPlugins = []
    , replaced = Nothing
    }


decoder : Decoder BootstrapConfig
decoder =
    D.succeed BootstrapConfig
        |> D.required "features" FeaturesConfig.decoder
        |> D.required "authentication" AuthenticationConfig.decoder
        |> D.required "loginScreen" LoginScreenSettings.decoder
        |> D.custom registryDecoder
        |> D.required "lookAndFeel" LookAndFeelConfig.decoder
        |> D.required "users" UsersSettings.decoder
        |> D.required "privacy" PrivacyConfig.decoder
        |> D.required "pluginSettings" (D.dict D.valueAsString)
        |> D.required "plugins" (D.list PluginInfo.decoder)
        |> D.required "cloud" CloudConfig.decoder
        |> D.required "tours" (D.list D.string)
        |> D.required "user" (D.nullable UserConfig.decoder)
        -- The organization's own custom menu links and announcements, where
        -- its support is and whether documents can be submitted, for its pages
        -- outside a workspace. How projects behave and which plugins are off
        -- are only ever a workspace's.
        |> D.required "dashboardAndMenu" DashboardAndMenuSettings.decoder
        |> D.hardcoded ProjectConfig.default
        |> D.required "support" SupportSettings.decoder
        |> D.required "submission" SubmissionConfig.decoder
        |> D.hardcoded []
        |> D.hardcoded Nothing


{-| A server that has a registry says so, and what it says has to be read. One
that has no registry section at all has nothing to switch on or off
(`RegistryConfig.unconfigured`).
-}
registryDecoder : Decoder RegistryConfig
registryDecoder =
    D.maybe (D.field "registry" D.value)
        |> D.andThen
            (\mbRegistry ->
                case mbRegistry of
                    Just _ ->
                        D.field "registry" RegistryConfig.decoder

                    Nothing ->
                        D.succeed RegistryConfig.unconfigured
            )


workspaceDecoder : Decoder WorkspaceConfig
workspaceDecoder =
    D.succeed WorkspaceConfig
        |> D.required "logoUrl" (D.nullable D.string)
        |> D.required "primaryColor" (D.nullable D.hexColor)
        |> D.required "dashboardAndMenu" DashboardAndMenuSettings.decoder
        |> D.required "projects" ProjectConfig.decoder
        |> D.required "support" SupportSettings.decoder
        |> D.required "submission" SubmissionConfig.decoder
        |> D.required "disabledPlugins" (D.list D.string)
        |> D.required "pluginSettings" (D.dict D.valueAsString)


{-| Lay the second part over the first. Its logo and color, where it has them,
and its plugin settings take the place of the first part's.
-}
withWorkspace : WorkspaceConfig -> BootstrapConfig -> BootstrapConfig
withWorkspace workspace config =
    let
        base =
            withoutWorkspace config

        lookAndFeel =
            base.lookAndFeel
    in
    { base
        | lookAndFeel =
            { lookAndFeel
                | logoUrl = Maybe.or workspace.logoUrl lookAndFeel.logoUrl
                , primaryColor = Maybe.or workspace.primaryColor lookAndFeel.primaryColor
            }
        , pluginSettings = workspace.pluginSettings
        , dashboardAndMenu = workspace.dashboardAndMenu
        , project = workspace.project
        , support = workspace.support
        , submission = workspace.submission
        , disabledPlugins = workspace.disabledPlugins
        , replaced = Just { lookAndFeel = base.lookAndFeel, pluginSettings = base.pluginSettings, dashboardAndMenu = base.dashboardAndMenu, support = base.support, submission = base.submission }
    }


{-| A logo and a color over the ones in force, where the place the user is in has
its own and they are known before — or apart from — the second part. Where it
has none, the second part's stay, and the first part's under them. Like the
second part's, they are taken away by `withoutWorkspace`.
-}
withLook : { logoUrl : Maybe String, primaryColor : Maybe Color } -> BootstrapConfig -> BootstrapConfig
withLook look config =
    let
        original =
            Maybe.withDefault { lookAndFeel = config.lookAndFeel, pluginSettings = config.pluginSettings, dashboardAndMenu = config.dashboardAndMenu, support = config.support, submission = config.submission } config.replaced

        lookAndFeel =
            config.lookAndFeel
    in
    { config
        | lookAndFeel =
            { lookAndFeel
                | logoUrl = Maybe.or look.logoUrl lookAndFeel.logoUrl
                , primaryColor = Maybe.or look.primaryColor lookAndFeel.primaryColor
            }
        , replaced = Just original
    }


{-| The first part's own look, whatever is laid over it: the instance's logo and
color while somebody is where another one applies.
-}
ownLookAndFeel : BootstrapConfig -> LookAndFeelConfig
ownLookAndFeel config =
    Maybe.unwrap config.lookAndFeel .lookAndFeel config.replaced


{-| Take the second part away again: its fields go back to their defaults, and
what it replaced comes back.
-}
withoutWorkspace : BootstrapConfig -> BootstrapConfig
withoutWorkspace config =
    case config.replaced of
        Just replaced ->
            { config
                | lookAndFeel = replaced.lookAndFeel
                , pluginSettings = replaced.pluginSettings
                , dashboardAndMenu = replaced.dashboardAndMenu
                , project = ProjectConfig.default
                , support = replaced.support
                , submission = replaced.submission
                , disabledPlugins = []
                , replaced = Nothing
            }

        Nothing ->
            config


{-| What the application calls an instance that has no title of its own.
-}
withDefaultAppTitle : String -> BootstrapConfig -> BootstrapConfig
withDefaultAppTitle defaultAppTitle config =
    { config
        | lookAndFeel = LookAndFeelConfig.withDefaultAppTitle defaultAppTitle config.lookAndFeel
        , replaced = Maybe.map (\replaced -> { replaced | lookAndFeel = LookAndFeelConfig.withDefaultAppTitle defaultAppTitle replaced.lookAndFeel }) config.replaced
    }


addTour : String -> BootstrapConfig -> BootstrapConfig
addTour tour config =
    { config
        | tours =
            if List.member tour config.tours then
                config.tours

            else
                tour :: config.tours
    }
