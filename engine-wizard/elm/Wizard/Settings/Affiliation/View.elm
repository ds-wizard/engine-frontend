module Wizard.Settings.Affiliation.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Affiliation.Models exposing (Model)
import Wizard.Settings.Affiliation.Msgs exposing (Msg)
import Wizard.Settings.Common.AffiliationConfigForm exposing (AffiliationConfigForm)
import Wizard.Settings.Generic.View as GenericView


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Affiliation.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Affiliation.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps AffiliationConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError AffiliationConfigForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.resizableTextarea appState form "affiliations" (l_ "form.affiliations" appState)
        , FormExtra.mdAfter (l_ "form.affiliations.desc" appState)
        ]
