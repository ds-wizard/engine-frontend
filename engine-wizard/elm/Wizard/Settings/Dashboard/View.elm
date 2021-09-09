module Wizard.Settings.Dashboard.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div, img, label, p, strong)
import Html.Attributes exposing (class, src)
import Shared.Form.FormError exposing (FormError)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.Forms.DashboardConfigForm as DashboardConfigForm exposing (DashboardConfigForm)
import Wizard.Settings.Dashboard.Models exposing (Model)
import Wizard.Settings.Generic.Msgs exposing (Msg)
import Wizard.Settings.Generic.View as GenericView


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Dashboard.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Dashboard.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps DashboardConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form FormError DashboardConfigForm -> Html Form.Msg
formView appState form =
    let
        opts =
            [ ( DashboardConfigForm.dashboardWelcome
              , div []
                    [ strong [] [ lx_ "dashboardOptions.welcome" appState ]
                    , p [ class "text-muted" ] [ lx_ "dashboardOptions.welcome.desc" appState ]
                    , img [ class "settings-img", src "/img/settings/dashboard-welcome.png" ] []
                    ]
              )
            , ( DashboardConfigForm.dashboardDmp
              , div []
                    [ strong [] [ lx_ "dashboardOptions.dmp" appState ]
                    , p [ class "text-muted" ] [ lx_ "dashboardOptions.dmp.desc" appState ]
                    , img [ class "settings-img", src "/img/settings/dashboard-dmp.png" ] []
                    ]
              )
            ]
    in
    div [ class "Dashboard" ]
        [ FormGroup.htmlRadioGroup appState opts form "widgets" (l_ "form.dashboardStyle" appState)
        , div [ class "row mt-5" ]
            [ div [ class "col-12" ]
                [ label [] [ lx_ "form.welcomeInfo" appState ]
                ]
            , div [ class "col-8" ]
                [ FormGroup.markdownEditor appState form "welcomeInfo" ""
                , FormExtra.mdAfter (l_ "form.welcomeInfo.desc" appState)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/welcome-info.png" ] []
                ]
            ]
        , div [ class "row mt-5" ]
            [ div [ class "col-12" ]
                [ label [] [ lx_ "form.welcomeWarning" appState ]
                ]
            , div [ class "col-8" ]
                [ FormGroup.markdownEditor appState form "welcomeWarning" ""
                , FormExtra.mdAfter (l_ "form.welcomeWarning.desc" appState)
                ]
            , div [ class "col-4" ]
                [ img [ class "settings-img", src "/img/settings/welcome-warning.png" ] []
                ]
            ]
        ]
