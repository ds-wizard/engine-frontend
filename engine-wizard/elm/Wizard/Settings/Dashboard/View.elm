module Wizard.Settings.Dashboard.View exposing (view)

import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div, img, label, p, strong, text)
import Html.Attributes exposing (class, src)
import Shared.Form.FormError exposing (FormError)
import Shared.Utils exposing (compose2)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.DashboardConfigForm as DashboardConfigForm exposing (DashboardConfigForm)
import Wizard.Settings.Dashboard.Models exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg(..))
import Wizard.Settings.Generic.View as GenericView


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps DashboardConfigForm Msg
viewProps =
    { locTitle = gettext "Dashboard"
    , locSave = gettext "Save"
    , formView = compose2 (Html.map FormMsg) formView
    , wrapMsg = FormMsg
    }


formView : AppState -> Form FormError DashboardConfigForm -> Html Form.Msg
formView appState form =
    let
        opts =
            [ ( DashboardConfigForm.dashboardWelcome
              , div []
                    [ strong [] [ text (gettext "Welcome" appState.locale) ]
                    , p [ class "text-muted" ] [ text (gettext "Standard welcome screen." appState.locale) ]
                    , img [ class "settings-img", src "/img/settings/dashboard-welcome.png" ] []
                    ]
              )
            , ( DashboardConfigForm.dashboardRoleBased
              , div []
                    [ strong [] [ text (gettext "Role-Based" appState.locale) ]
                    , p [ class "text-muted" ] [ text (gettext "Relevant content based on user's role." appState.locale) ]
                    , img [ class "settings-img", src "/img/settings/dashboard-rolebased.png" ] []
                    ]
              )
            ]
    in
    div [ class "Dashboard" ]
        [ FormGroup.htmlRadioGroup appState opts form "dashboardType" (gettext "Dashboard Style" appState.locale)
        , div [ class "row mt-5" ]
            [ div [ class "col-12" ]
                [ label [] [ text (gettext "Welcome Info" appState.locale) ]
                ]
            , div [ class "col-8" ]
                [ FormGroup.markdownEditor appState form "welcomeInfo" ""
                , FormExtra.mdAfter (gettext "Welcome info is visible at the dashboard after login as a blue box." appState.locale)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/welcome-info.png" ] []
                ]
            ]
        , div [ class "row mt-5" ]
            [ div [ class "col-12" ]
                [ label [] [ text (gettext "Welcome Warning" appState.locale) ]
                ]
            , div [ class "col-8" ]
                [ FormGroup.markdownEditor appState form "welcomeWarning" ""
                , FormExtra.mdAfter (gettext "Welcome warning is visible at the dashboard after login as a yellow box." appState.locale)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/welcome-warning.png" ] []
                ]
            ]
        ]
