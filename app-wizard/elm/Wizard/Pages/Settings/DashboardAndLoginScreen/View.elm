module Wizard.Pages.Settings.DashboardAndLoginScreen.View exposing (view)

import Common.Components.FontAwesome exposing (faDelete)
import Common.Components.FormExtra as FormExtra
import Common.Components.FormGroup as FormGroup
import Common.Components.Tooltip exposing (tooltip)
import Common.Utils.Form.FormError exposing (FormError)
import Compose exposing (compose2)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h3, hr, img, p, strong, text)
import Html.Attributes exposing (class, src)
import Html.Attributes.Extensions exposing (dataCy)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Wizard.Api.Models.BootstrapConfig.Admin as Admin
import Wizard.Api.Models.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement.AnnouncementLevel as AnnouncementLevel
import Wizard.Data.AppState exposing (AppState)
import Wizard.Pages.Settings.Common.Forms.DashboardAndLoginScreenConfigForm as DashboardAndLoginScreenConfigForm exposing (DashboardAndLoginScreenConfigForm)
import Wizard.Pages.Settings.DashboardAndLoginScreen.Models exposing (Model)
import Wizard.Pages.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Pages.Settings.Generic.View as GenericView
import Wizard.Utils.WizardGuideLinks as WizardGuideLinks


view : AppState -> Model -> Html Msg
view appState =
    GenericView.view (viewProps appState) appState


viewProps : AppState -> GenericView.ViewProps DashboardAndLoginScreenConfigForm Msg
viewProps appState =
    let
        locTitle =
            if Admin.isEnabled appState.config.admin then
                gettext "Dashboard"

            else
                gettext "Dashboard & Login Screen"
    in
    { locTitle = locTitle
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , guideLink = WizardGuideLinks.settingsDashboardAndLoginScreen
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError DashboardAndLoginScreenConfigForm -> Html Form.Msg
formView appState form =
    let
        opts =
            [ ( DashboardAndLoginScreenConfigForm.dashboardWelcome
              , div []
                    [ strong [] [ text (gettext "Welcome" appState.locale) ]
                    , p [ class "text-muted" ] [ text (gettext "Standard welcome screen." appState.locale) ]
                    , img [ class "settings-img", src "/wizard/assets/settings/dashboard-welcome.png" ] []
                    ]
              )
            , ( DashboardAndLoginScreenConfigForm.dashboardRoleBased
              , div []
                    [ strong [] [ text (gettext "Role-Based" appState.locale) ]
                    , p [ class "text-muted" ] [ text (gettext "Relevant content based on user's role." appState.locale) ]
                    , img [ class "settings-img", src "/wizard/assets/settings/dashboard-rolebased.png" ] []
                    ]
              )
            ]

        loginInfos =
            if Admin.isEnabled appState.config.admin then
                []

            else
                [ hr [] []
                , div [ class "row mt-5" ]
                    [ div [ class "col-8" ]
                        [ FormGroup.htmlOrMarkdownEditor appState.locale (WizardGuideLinks.markdownCheatsheet appState.guideLinks) form "loginInfo" (gettext "Login Info" appState.locale)
                        , FormExtra.mdAfter (gettext "Additional information displayed on the login screen next to the login form." appState.locale)
                        ]
                    , div [ class "col-4" ]
                        [ img [ class "settings-img", src "/wizard/assets/settings/login-info-text.png" ] []
                        ]
                    ]
                , div [ class "row mt-5" ]
                    [ div [ class "col-8" ]
                        [ FormGroup.htmlOrMarkdownEditor appState.locale (WizardGuideLinks.markdownCheatsheet appState.guideLinks) form "loginInfoSidebar" (gettext "Sidebar Login Info" appState.locale)
                        , FormExtra.mdAfter (gettext "Additional information displayed on the login screen underneath the login form." appState.locale)
                        ]
                    , div [ class "col-4" ]
                        [ img [ class "settings-img", src "/wizard/assets/settings/login-info-sidebar-text.png" ] []
                        ]
                    ]
                ]

        announcements =
            if Admin.isEnabled appState.config.admin then
                []

            else
                [ hr [] []
                , h3 [] [ text (gettext "Announcements" appState.locale) ]
                , div [ class "row mt-5" ]
                    [ div [ class "col-8" ]
                        [ FormGroup.list appState.locale
                            (announcementFormView appState)
                            form
                            "announcements"
                            (gettext "Announcements" appState.locale)
                            (gettext "Add announcement" appState.locale)
                        ]
                    , div [ class "col-4" ]
                        [ img [ class "settings-img", src "/wizard/assets/settings/announcements.png" ] []
                        ]
                    ]
                ]
    in
    div [ class "Dashboard" ]
        (FormGroup.htmlRadioGroup appState.locale opts form "dashboardType" (gettext "Dashboard Style" appState.locale)
            :: loginInfos
            ++ announcements
        )


announcementFormView : AppState -> Form FormError DashboardAndLoginScreenConfigForm -> Int -> Html Form.Msg
announcementFormView appState form i =
    let
        field fieldName =
            "announcements." ++ String.fromInt i ++ "." ++ fieldName

        contentField =
            field "content"

        levelField =
            field "level"

        dashboardField =
            field "dashboard"

        loginScreenField =
            field "loginScreen"

        levelOptions =
            [ ( AnnouncementLevel.toString AnnouncementLevel.Info, gettext "Info" appState.locale, "info" )
            , ( AnnouncementLevel.toString AnnouncementLevel.Warning, gettext "Warning" appState.locale, "warning" )
            , ( AnnouncementLevel.toString AnnouncementLevel.Critical, gettext "Critical" appState.locale, "danger" )
            ]
    in
    div [ class "card bg-light mb-4" ]
        [ div [ class "card-body" ]
            [ div [ class "text-end" ]
                [ a
                    ([ class "text-danger"
                     , onClick (Form.RemoveItem "announcements" i)
                     , dataCy "settings_announcements_remove-button"
                     ]
                        ++ tooltip (gettext "Remove announcement" appState.locale)
                    )
                    [ faDelete
                    ]
                ]
            , FormGroup.alertRadioGroup appState.locale levelOptions form levelField (gettext "Level" appState.locale)
            , FormGroup.markdownEditor appState.locale (WizardGuideLinks.markdownCheatsheet appState.guideLinks) form contentField (gettext "Content" appState.locale)
            , FormGroup.toggle form dashboardField (gettext "Dashboard" appState.locale)
            , FormExtra.textAfter (gettext "Display on the dashboard after users log in." appState.locale)
            , Html.viewIf (not (Admin.isEnabled appState.config.admin)) <|
                FormGroup.toggle form loginScreenField (gettext "Login Screen" appState.locale)
            , Html.viewIf (not (Admin.isEnabled appState.config.admin)) <|
                FormExtra.textAfter (gettext "Display on the login screen, visible to everybody." appState.locale)
            ]
        ]
