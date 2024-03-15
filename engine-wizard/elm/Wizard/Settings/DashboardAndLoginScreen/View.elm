module Wizard.Settings.DashboardAndLoginScreen.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, a, div, h3, hr, img, p, strong, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Shared.Data.BootstrapConfig.Admin as Admin
import Shared.Data.BootstrapConfig.DashboardAndLoginScreenConfig.Announcement.AnnouncementLevel as AnnouncementLevel
import Shared.Form.FormError exposing (FormError)
import Shared.Html exposing (faSet)
import Shared.Utils exposing (compose2)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Html.Attribute exposing (dataCy, tooltip)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.DashboardAndLoginScreenConfigForm as DashboardAndLoginScreenConfigForm exposing (DashboardAndLoginScreenConfigForm)
import Wizard.Settings.DashboardAndLoginScreen.Models exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView


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
                    , img [ class "settings-img", src "/wizard/img/settings/dashboard-welcome.png" ] []
                    ]
              )
            , ( DashboardAndLoginScreenConfigForm.dashboardRoleBased
              , div []
                    [ strong [] [ text (gettext "Role-Based" appState.locale) ]
                    , p [ class "text-muted" ] [ text (gettext "Relevant content based on user's role." appState.locale) ]
                    , img [ class "settings-img", src "/wizard/img/settings/dashboard-rolebased.png" ] []
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
                        [ FormGroup.htmlOrMarkdownEditor appState form "loginInfo" (gettext "Login Info" appState.locale)
                        , FormExtra.mdAfter (gettext "Additional information displayed on the login screen next to the login form." appState.locale)
                        ]
                    , div [ class "col-4" ]
                        [ img [ class "settings-img", src "/wizard/img/settings/login-info-text.png" ] []
                        ]
                    ]
                , div [ class "row mt-5" ]
                    [ div [ class "col-8" ]
                        [ FormGroup.htmlOrMarkdownEditor appState form "loginInfoSidebar" (gettext "Sidebar Login Info" appState.locale)
                        , FormExtra.mdAfter (gettext "Additional information displayed on the login screen underneath the login form." appState.locale)
                        ]
                    , div [ class "col-4" ]
                        [ img [ class "settings-img", src "/wizard/img/settings/login-info-sidebar-text.png" ] []
                        ]
                    ]
                ]
    in
    div [ class "Dashboard" ]
        (FormGroup.htmlRadioGroup appState opts form "dashboardType" (gettext "Dashboard Style" appState.locale)
            :: loginInfos
            ++ [ hr [] []
               , h3 [] [ text (gettext "Announcements" appState.locale) ]
               , div [ class "row mt-5" ]
                    [ div [ class "col-8" ]
                        [ FormGroup.list appState
                            (announcementFormView appState)
                            form
                            "announcements"
                            (gettext "Announcements" appState.locale)
                            (gettext "Add announcement" appState.locale)
                        ]
                    , div [ class "col-4" ]
                        [ img [ class "settings-img", src "/wizard/img/settings/announcements.png" ] []
                        ]
                    ]
               ]
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
                    [ faSet "_global.delete" appState
                    ]
                ]
            , FormGroup.alertRadioGroup appState levelOptions form levelField (gettext "Level" appState.locale)
            , FormGroup.markdownEditor appState form contentField (gettext "Content" appState.locale)
            , FormGroup.toggle form dashboardField (gettext "Dashboard" appState.locale)
            , FormExtra.textAfter (gettext "Display on the dashboard after users log in." appState.locale)
            , Html.viewIf (not (Admin.isEnabled appState.config.admin)) <|
                FormGroup.toggle form loginScreenField (gettext "Login Screen" appState.locale)
            , Html.viewIf (not (Admin.isEnabled appState.config.admin)) <|
                FormExtra.textAfter (gettext "Display on the login screen, visible to everybody." appState.locale)
            ]
        ]
