module Wizard.Settings.Info.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.InfoConfigForm exposing (InfoConfigForm)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Info.Models exposing (Model)
import Wizard.Settings.Info.Msgs exposing (Msg)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Info.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Info.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps InfoConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError InfoConfigForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.markdownEditor appState form "welcomeInfo" (l_ "form.welcomeInfo" appState)
        , FormExtra.mdAfter (l_ "form.welcomeInfo.desc" appState)
        , FormGroup.markdownEditor appState form "welcomeWarning" (l_ "form.welcomeWarning" appState)
        , FormExtra.mdAfter (l_ "form.welcomeWarning.desc" appState)
        , FormGroup.markdownEditor appState form "loginInfo" (l_ "form.loginInfo" appState)
        , FormExtra.mdAfter (l_ "form.loginInfo.desc" appState)
        ]
