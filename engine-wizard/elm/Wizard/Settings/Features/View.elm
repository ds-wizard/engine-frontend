module Wizard.Settings.Features.View exposing (view)

import Form exposing (Form)
import Html exposing (Html, div)
import Shared.Locale exposing (l, lx)
import Wizard.Common.AppState exposing (AppState)
import Wizard.Common.Form exposing (CustomFormError)
import Wizard.Common.View.FormExtra as FormExtra
import Wizard.Common.View.FormGroup as FormGroup
import Wizard.Settings.Common.FeaturesConfigForm exposing (FeaturesConfigForm)
import Wizard.Settings.Features.Models exposing (Model)
import Wizard.Settings.Features.Msgs exposing (Msg)
import Wizard.Settings.Generic.View as GenericView


l_ : String -> AppState -> String
l_ =
    l "Wizard.Settings.Features.View"


lx_ : String -> AppState -> Html msg
lx_ =
    lx "Wizard.Settings.Features.View"


view : AppState -> Model -> Html Msg
view =
    GenericView.view viewProps


viewProps : GenericView.ViewProps FeaturesConfigForm
viewProps =
    { locTitle = l_ "title"
    , locSave = l_ "save"
    , formView = formView
    }


formView : AppState -> Form CustomFormError FeaturesConfigForm -> Html Form.Msg
formView appState form =
    div []
        [ FormGroup.toggle form "publicQuestionnaireEnabled" (l_ "form.publicQuestionnaire" appState)
        , FormExtra.mdAfter (l_ "form.publicQuestionnaire.desc" appState)
        , FormGroup.toggle form "questionnaireAccessibilityEnabled" (l_ "form.questionnaireAccessibility" appState)
        , FormExtra.mdAfter (l_ "form.questionnaireAccessibility.desc" appState)
        , FormGroup.toggle form "levelsEnabled" (l_ "form.phases" appState)
        , FormExtra.mdAfter (l_ "form.phases.desc" appState)
        , FormGroup.toggle form "registrationEnabled" (l_ "form.registration" appState)
        , FormExtra.mdAfter (l_ "form.registration.desc" appState)
        ]
