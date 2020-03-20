module Wizard.Settings.Organization.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.OrganizationConfigForm exposing (OrganizationConfigForm)
import Wizard.Settings.Generic.View as GenericView
import Wizard.Settings.Organization.Models exposing (Model)
import Wizard.Settings.Organization.Msgs exposing (Msg)


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Organization.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Organization.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps OrganizationConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError OrganizationConfigForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.input appState form "name" (l_ "form.name" appState)
        , FormExtra.textAfter (l_ "form.name.desc" appState)
        , FormGroup.input appState form "organizationId" (l_ "form.organizationId" appState)
        , FormExtra.textAfter (l_ "form.organizationId.desc" appState)
        ]
