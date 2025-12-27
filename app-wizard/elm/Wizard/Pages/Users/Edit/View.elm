module Wizard.Pages.Users.Edit.View exposing (view)

import Gettext exposing (gettext)
import Html exposing (Html, div, strong, text)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Extra as Html
import Maybe.Extra as Maybe
import Uuid
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Components.Html exposing (linkTo)
import Wizard.Data.AppState as AppState exposing (AppState)
import Wizard.Pages.Users.Edit.Components.ActiveSessions as ActiveSessions
import Wizard.Pages.Users.Edit.Components.ApiKeys as ApiKeys
import Wizard.Pages.Users.Edit.Components.AppKeys as AppKeys
import Wizard.Pages.Users.Edit.Components.Language as Language
import Wizard.Pages.Users.Edit.Components.Password as Password
import Wizard.Pages.Users.Edit.Components.PluginSettings as PluginSettings
import Wizard.Pages.Users.Edit.Components.Profile as Profile
import Wizard.Pages.Users.Edit.Components.SubmissionSettings as SubmissionSettings
import Wizard.Pages.Users.Edit.Components.Tours as Tours
import Wizard.Pages.Users.Edit.Models exposing (Model)
import Wizard.Pages.Users.Edit.Msgs exposing (Msg(..))
import Wizard.Pages.Users.Edit.UserEditRoutes as UserEditRoutes exposing (UserEditRoute)
import Wizard.Routes as Routes
import Wizard.Utils.Feature as Feature
import Wizard.Utils.HtmlAttributesUtils exposing (settingsClass)


view : AppState -> UserEditRoute -> Model -> Html Msg
view appState subroute model =
    let
        content =
            case subroute of
                UserEditRoutes.Profile ->
                    Html.map ProfileMsg <|
                        Profile.view appState model.profileModel

                UserEditRoutes.Password ->
                    Html.map PasswordMsg <|
                        Password.view appState model.passwordModel

                UserEditRoutes.Language ->
                    Html.map LanguageMsg <|
                        Language.view appState model.languageModel

                UserEditRoutes.Tours ->
                    Html.map ToursMsg <|
                        Tours.view appState model.toursModel

                UserEditRoutes.ApiKeys ->
                    Html.map ApiKeysMsg <|
                        ApiKeys.view appState model.apiKeysModel

                UserEditRoutes.AppKeys ->
                    Html.map AppKeysMsg <|
                        AppKeys.view appState model.appKeysModel

                UserEditRoutes.ActiveSessions ->
                    Html.map ActiveSessionsMsg <|
                        ActiveSessions.view appState model.activeSessionsModel

                UserEditRoutes.SubmissionSettings ->
                    Html.map SubmissionSettingsMsg <|
                        SubmissionSettings.view appState model.submissionSettingsModel

                UserEditRoutes.PluginSettings _ ->
                    Html.map PluginSettingsMsg <|
                        PluginSettings.view appState model.pluginSettingsModel
    in
    div [ settingsClass "Users__Edit" ]
        [ div [ class "Users__Edit__navigation" ] [ navigation appState subroute model ]
        , div [ class "Users__Edit__content" ]
            [ content ]
        ]


navigation : AppState -> UserEditRoute -> Model -> Html Msg
navigation appState subroute model =
    let
        pluginLink plugin =
            linkTo (Routes.usersEditPluginSettings model.uuidOrCurrent plugin.uuid)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.PluginSettings plugin.uuid ) ]
                , dataCy ("user_nav_plugin_" ++ Uuid.toString plugin.uuid)
                ]
                [ text plugin.name ]

        pluginLinks =
            AppState.getPlugins appState
                |> List.filter (Maybe.isJust << .userSettings << .connectors)
                |> List.map pluginLink

        plugins =
            if Feature.userEditPlugins appState model.uuidOrCurrent && not (List.isEmpty pluginLinks) then
                strong [] [ text (gettext "Plugins" appState.locale) ]
                    :: pluginLinks

            else
                []
    in
    div [ class "nav nav-pills flex-column" ]
        ([ strong [] [ text (gettext "User Settings" appState.locale) ]
         , linkTo (Routes.usersEdit model.uuidOrCurrent)
            [ class "nav-link"
            , classList [ ( "active", subroute == UserEditRoutes.Profile ) ]
            , dataCy "user_nav_profile"
            ]
            [ text (gettext "Profile" appState.locale)
            ]
         , Html.viewIf (not (Admin.isEnabled appState.config.admin))
            (linkTo (Routes.usersEditPassword model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.Password ) ]
                , dataCy "user_nav_password"
                ]
                [ text (gettext "Password" appState.locale)
                ]
            )
         , Html.viewIf (not (Admin.isEnabled appState.config.admin) && Feature.userEditLanguage appState model.uuidOrCurrent)
            (linkTo (Routes.usersEditLanguage model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.Language ) ]
                , dataCy "user_nav_language"
                ]
                [ text (gettext "Language" appState.locale)
                ]
            )
         , Html.viewIf (appState.config.features.toursEnabled && not (Admin.isEnabled appState.config.admin) && Feature.userEditTours appState model.uuidOrCurrent)
            (linkTo (Routes.usersEditTours model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.Tours ) ]
                , dataCy "user_nav_tours"
                ]
                [ text (gettext "Tours" appState.locale)
                ]
            )
         , Html.viewIf (Feature.userEditApiKeys appState model.uuidOrCurrent)
            (linkTo (Routes.usersEditApiKeys model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.ApiKeys ) ]
                , dataCy "user_nav_api-keys"
                ]
                [ text (gettext "API Keys" appState.locale)
                ]
            )
         , Html.viewIf (Feature.userEditAppKeys appState model.uuidOrCurrent)
            (linkTo (Routes.usersEditAppKeys model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.AppKeys ) ]
                , dataCy "user_nav_app-keys"
                ]
                [ text (gettext "App Keys" appState.locale)
                ]
            )
         , Html.viewIf (Feature.userEditActiveSessions appState model.uuidOrCurrent)
            (linkTo (Routes.usersEditActiveSessions model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.ActiveSessions ) ]
                , dataCy "user_nav_active-sessions"
                ]
                [ text (gettext "Active Sessions" appState.locale)
                ]
            )
         , Html.viewIf (Feature.userEditSubmissionSettings appState model.uuidOrCurrent)
            (linkTo (Routes.usersEditSubmissionSettings model.uuidOrCurrent)
                [ class "nav-link"
                , classList [ ( "active", subroute == UserEditRoutes.SubmissionSettings ) ]
                , dataCy "user_nav_submission-settings"
                ]
                [ text (gettext "Submission Settings" appState.locale)
                ]
            )
         ]
            ++ plugins
        )
